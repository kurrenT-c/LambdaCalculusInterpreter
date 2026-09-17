
#include <cstring>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace lc {

enum class Tag { Var, Lam, App };

struct Expr;
using ExprPtr = Expr *;

struct Expr {
    Tag tag;
    std::string name;
    ExprPtr a, b;
};

class Arena {
public:
    Expr *alloc() {
        if (used_ >= kBlockSize) {
            blocks_.push_back(std::make_unique<Expr[]>(kBlockSize));
            used_ = 0;
        }
        return &blocks_.back()[used_++];
    }

    void reset() {
        blocks_.clear();
        used_ = kBlockSize; 
    }

private:
    static constexpr size_t kBlockSize = 1 << 16; 
    std::vector<std::unique_ptr<Expr[]>> blocks_;
    size_t used_ = kBlockSize;
};

Arena g_arena;

ExprPtr mkVar(std::string n) {
    Expr *e = g_arena.alloc();
    e->tag = Tag::Var;
    e->name = std::move(n);
    e->a = e->b = nullptr;
    return e;
}

ExprPtr mkLam(std::string n, ExprPtr body) {
    Expr *e = g_arena.alloc();
    e->tag = Tag::Lam;
    e->name = std::move(n);
    e->a = body;
    e->b = nullptr;
    return e;
}

ExprPtr mkApp(ExprPtr f, ExprPtr arg) {
    Expr *e = g_arena.alloc();
    e->tag = Tag::App;
    e->a = f;
    e->b = arg;
    return e;
}



bool isIdentChar(char c) {
    return std::isalnum(static_cast<unsigned char>(c)) || c == '_' || c == '\'';
}

std::vector<std::string> tokenize(const std::string &src) {
    std::vector<std::string> toks;
    size_t i = 0;
    while (i < src.size()) {
        char c = src[i];
        if (std::isspace(static_cast<unsigned char>(c))) {
            ++i;
        } else if (c == '\\' || c == '.' || c == '(' || c == ')') {
            toks.emplace_back(1, c);
            ++i;
        } else if (isIdentChar(c)) {
            size_t start = i;
            while (i < src.size() && isIdentChar(src[i])) ++i;
            toks.push_back(src.substr(start, i - start));
        } else {
            std::ostringstream msg;
            msg << "unexpected character: " << c;
            throw std::runtime_error(msg.str());
        }
    }
    return toks;
}



class Parser {
public:
    explicit Parser(std::vector<std::string> toks) : toks_(std::move(toks)), pos_(0) {}

    ExprPtr parseAll() {
        ExprPtr e = parseExpr();
        if (pos_ != toks_.size()) {
            throw std::runtime_error("unexpected trailing tokens");
        }
        return e;
    }

private:
    std::vector<std::string> toks_;
    size_t pos_;

    bool atEnd() const { return pos_ >= toks_.size(); }
    const std::string &peek() const { return toks_[pos_]; }

    ExprPtr parseExpr() {
        if (!atEnd() && (peek() == "\\")) return parseLambda();
        return parseApp();
    }

    ExprPtr parseLambda() {
        ++pos_;  // consume '\'
        std::vector<std::string> binders;
        while (!atEnd() && peek() != ".") {
            binders.push_back(peek());
            ++pos_;
        }
        if (binders.empty()) throw std::runtime_error("expected at least one binder after '\\'");
        if (atEnd() || peek() != ".") throw std::runtime_error("expected '.' after lambda binders");
        ++pos_;  // consume '.'
        ExprPtr body = parseExpr();
        for (auto it = binders.rbegin(); it != binders.rend(); ++it) {
            body = mkLam(*it, body);
        }
        return body;
    }

    ExprPtr parseApp() {
        ExprPtr acc = parseAtom();
        while (!atEnd() && (peek() == "(" || isIdentChar(peek()[0]))) {
            acc = mkApp(acc, parseAtom());
        }
        return acc;
    }

    ExprPtr parseAtom() {
        if (atEnd()) throw std::runtime_error("expected a variable or '('");
        if (peek() == "(") {
            ++pos_;
            ExprPtr e = parseExpr();
            if (atEnd() || peek() != ")") throw std::runtime_error("expected closing ')'");
            ++pos_;
            return e;
        }
        if (peek() == "." || peek() == ")" || peek() == "\\") {
            throw std::runtime_error("expected a variable or '('");
        }
        std::string name = peek();
        ++pos_;
        return mkVar(name);
    }
};

ExprPtr parse(const std::string &src) {
    return Parser(tokenize(src)).parseAll();
}


using NameSet = std::vector<std::string>;

bool contains(const NameSet &s, const std::string &name) {
    for (const auto &n : s) {
        if (n == name) return true;
    }
    return false;
}

void insertUnique(NameSet &s, const std::string &name) {
    if (!contains(s, name)) s.push_back(name);
}

void freeVars(const ExprPtr &e, NameSet &out) {
    switch (e->tag) {
        case Tag::Var:
            insertUnique(out, e->name);
            break;
        case Tag::Lam: {
            NameSet inner;
            freeVars(e->a, inner);
            for (const auto &n : inner) {
                if (n != e->name) insertUnique(out, n);
            }
            break;
        }
        case Tag::App:
            freeVars(e->a, out);
            freeVars(e->b, out);
            break;
    }
}

std::string fresh(const std::string &base, const NameSet &avoid) {
    std::string candidate = base;
    while (contains(avoid, candidate)) {
        candidate += '\'';
    }
    return candidate;
}

ExprPtr rename(const std::string &oldName, const std::string &newName, const ExprPtr &e) {
    switch (e->tag) {
        case Tag::Var:
            return e->name == oldName ? mkVar(newName) : e;
        case Tag::Lam:
            if (e->name == oldName) {
                return mkLam(newName, rename(oldName, newName, e->a));
            }
            return mkLam(e->name, rename(oldName, newName, e->a));
        case Tag::App:
            return mkApp(rename(oldName, newName, e->a), rename(oldName, newName, e->b));
    }
    throw std::logic_error("unreachable");
}

ExprPtr subst(const std::string &x, const ExprPtr &n, const ExprPtr &target) {
    switch (target->tag) {
        case Tag::Var:
            return target->name == x ? n : target;
        case Tag::App:
            return mkApp(subst(x, n, target->a), subst(x, n, target->b));
        case Tag::Lam: {
            if (target->name == x) return target;  // shadowed
            NameSet freeN;
            freeVars(n, freeN);
            if (contains(freeN, target->name)) {
                NameSet avoid;
                freeVars(target->a, avoid);
                for (const auto &v : freeN) insertUnique(avoid, v);
                std::string y2 = fresh(target->name, avoid);
                ExprPtr renamedBody = rename(target->name, y2, target->a);
                return mkLam(y2, subst(x, n, renamedBody));
            }
            return mkLam(target->name, subst(x, n, target->a));
        }
    }
    throw std::logic_error("unreachable");
}



// Returns the reduced expression and whether a reduction actually
// happened (mirrors Haskell's Maybe Expr via a bool flag).
std::pair<ExprPtr, bool> step(const ExprPtr &e) {
    if (e->tag == Tag::App) {
        if (e->a->tag == Tag::Lam) {
            return {subst(e->a->name, e->b, e->a->a), true};
        }
        auto [f2, changed] = step(e->a);
        if (changed) return {mkApp(f2, e->b), true};
        auto [a2, changed2] = step(e->b);
        if (changed2) return {mkApp(e->a, a2), true};
        return {e, false};
    }
    if (e->tag == Tag::Lam) {
        auto [body2, changed] = step(e->a);
        if (changed) return {mkLam(e->name, body2), true};
        return {e, false};
    }
    return {e, false};  // Var: already normal
}

ExprPtr normalize(ExprPtr e, int maxSteps) {
    for (int i = 0; i < maxSteps; ++i) {
        auto [next, changed] = step(e);
        if (!changed) return e;
        e = next;
    }
    return e;
}


void printExpr(const ExprPtr &e, int prec, std::ostringstream &out) {
    switch (e->tag) {
        case Tag::Var:
            out << e->name;
            break;
        case Tag::Lam: {
            bool paren = prec > 0;
            if (paren) out << "(";
            out << "\\" << e->name << ". ";
            printExpr(e->a, 0, out);
            if (paren) out << ")";
            break;
        }
        case Tag::App: {
            bool paren = prec > 1;
            if (paren) out << "(";
            printExpr(e->a, 1, out);
            out << " ";
            printExpr(e->b, 2, out);
            if (paren) out << ")";
            break;
        }
    }
}

std::string prettyPrint(const ExprPtr &e) {
    std::ostringstream out;
    printExpr(e, 0, out);
    return out.str();
}

}

extern "C" char *lc_normalize(const char *src, int maxSteps) {
    std::string result;
    try {
        lc::ExprPtr e = lc::parse(std::string(src));
        e = lc::normalize(e, maxSteps);
        result = lc::prettyPrint(e);
    } catch (const std::exception &ex) {
        result = std::string("error: ") + ex.what();
    }
    lc::g_arena.reset();
    char *out = new char[result.size() + 1];
    std::memcpy(out, result.c_str(), result.size() + 1);
    return out;
}

extern "C" void lc_free(char *p) { delete[] p; }