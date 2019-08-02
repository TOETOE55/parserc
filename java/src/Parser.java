import java.util.function.Function;
import java.util.stream.Stream;

class Unit{}

class ParseError extends Exception {}

class Location {
    private String source;
    private Integer offset;
    private Integer len;

    Location(String s){
        source = s;
        offset = 0;
        len = source.length();
    }

    Location(String s, Integer p){
        source = s;
        offset = p;
        len = source.length();
    }

    String getSource() {
        return source;
    }

    Integer getOffset() {
        return offset;
    }

    Integer getLen() {
        return len;
    }

    Location advanceBy(Integer n) {
        return new Location(source, offset +n);
    }
}

abstract class Result<T> {
    abstract Result<T> orElse(Result<T> other);
    abstract <R> Result<R> next(Function<T, Parser<R>> f, Location s);
    abstract Result<T> uncommit();
    abstract Result<T> addCommit(Boolean isCommitted);
    abstract Result<T> advanceSuccess(Integer n);
    abstract public T getResult() throws ParseError;
    abstract public Integer getConsumed() throws ParseError;
}

class Successed<T> extends Result<T> {
    private final T result;
    private final Integer consumed;
    Successed(T res, Integer con) {
        result = res;
        consumed = con;
    }
    public T getResult() throws ParseError {
        return result;
    }
    public Integer getConsumed() throws ParseError{
        return consumed;
    }
    Result<T> orElse(Result<T> other) {
        return this;
    }
    <R> Result<R> next(Function<T, Parser<R>> f, Location s) {
        return f.apply(result).parse(s.advanceBy(consumed))
                .addCommit(consumed!=0).advanceSuccess(consumed);
    }
    Result<T> uncommit() {
        return this;
    }
    Result<T> addCommit(Boolean isCommitted) {
        return this;
    }
    Result<T> advanceSuccess(Integer n) {
        return new Successed<>(result, consumed+n);
    }
}

class Failed<T> extends Result<T> {
    private Boolean commit;
    Failed() {
        this.commit = true;
    }
    Failed(Boolean iscommit) {
        this.commit = iscommit;
    }
    public T getResult() throws ParseError {
        throw new ParseError();
    }
    public Integer getConsumed() throws ParseError {
        throw new ParseError();
    }
    Result<T> orElse(Result<T> other) {
        if (commit) {
            return this;
        } else {
            return other;
        }
    }
    <R> Result<R> next(Function<T, Parser<R>> f, Location s) {
        return new Failed<>(commit);
    }
    Result<T> uncommit() {
        if (commit) {
            return new Failed<>(false);
        } else {
            return this;
        }
    }
    Result<T> addCommit(Boolean isCommitted) {
        return new Failed<>(commit || isCommitted);
    }
    Result<T> advanceSuccess(Integer n) {
        return this;
    }
}

@FunctionalInterface
public interface Parser<T> {
    Result<T> parse(Location taget);

    //basic combinator
    /*
     * 1 p is a Parser such as Char.eat('a') which is to match an 'a'
     *   thus p.run("abc") = Successed('a', 1)
     *                                   /|\
     *             Consumed 1 char--------+
     *        p.run("bc") = Failed(...)
     *        p.run("") = Failed(...)
     *
     * 2 or is a basic combinator, where p,q is Parser:
     *   p.or(q) represent the BNF Normal form that p | q
     *   where
     *   p.or(q).run(s) = p.run(s) .orElse( q.run(s) )
     *
     * 3 unit, bind are also basic combinators
     *   which is to connect two parse and calculate some intermediate result.
     *   p.bind(_->q) represent the BNF Normal form that p q
     *   p.bind(_->q.bind(_->r)) represent p q r
     *   p.bind(x->q), the x is the result of p
     *   where
     *   unit(x).bind(f) = f.apply(x) : left identity element
     *   p.bind(x->unit(x)) = p : right identity element
     *   p.bind(f).bind(g) = p.bind(x->f(x).bind(g))
     *
     * */

    default Result<T> run(String s) {
        return parse(new Location(s));
    }
    static <T> Parser<T> unit(T res) {
        return s -> new Successed<>(res, 0);
    }
    default <R> Parser<R> bind(Function<T, Parser<R>> f) {
        return s -> this.parse(s).next(f, s);
    }
    default Parser<T> or(Parser<T> other){
        return s -> this.parse(s).orElse(other.parse(s));
    }
    default Parser<T> attempt() {
        return s->this.parse(s).uncommit();
    }
    public static <T> Parser<T> failed() {
        return str -> new Failed<>();
    }
    default Parser<T> done() {
        return s->{
            try {
                Result<T> result = this.parse(s);
                if (result.getConsumed().equals(s.getLen())) {
                    return result;
                } else {
                    return new Failed<>();
                }
            } catch (ParseError e) {
                return new Failed<>();
            }
        };
    }
}

//some combinators

class Char {
    static Parser<Character> eat(Character c) {
        return s-> {
            if (s.getOffset()+1>s.getLen()) {
                return new Failed<>();
            } else {
                if (s.getSource().charAt(s.getOffset()) == c) {
                    return new Successed<>(c, 1);
                } else {
                    return new Failed<>();
                }
            }
        };
    }
}

class Digit {
    //[0-9]
    static Parser<Integer> eat() {
        return s-> {
            if (s.getOffset()+1>s.getLen()) {
                return new Failed<>();
            } else {
                char digit = s.getSource().charAt(s.getOffset());
                if ('0'<=digit && digit<='9') {
                    return new Successed<>(digit-'0', 1);
                } else {
                    return new Failed<>();
                }
            }
        };
    }
}

class ManyOrSome {
    //p+
    static <T> Parser<Stream<T>> some(Parser<T> p) {
        return  p.bind(s->
                many(p).bind(ss->
                        Parser.unit(Stream.concat(Stream.of(s),ss))));
    }
    //p*
    static <T> Parser<Stream<T>> many(Parser<T> p) {
        return  some(p).attempt().or(Parser.unit(Stream.empty()));
    }

    static <T> Parser<String> some_(Parser<T> p) {
        return  p.bind(s->
                many_(p).bind(ss->
                        Parser.unit(s+ss)));
    }
    //p*
    static <T> Parser<String> many_(Parser<T> p) {
        return  some_(p).attempt().or(Parser.unit(""));
    }
}

class Space {
    static Parser<Unit> eat() {
        return  ManyOrSome.many(Char.eat(' '))
                .bind(u->Parser.unit(new Unit()));
    }
    static <T> Parser<T> lexeme(Parser<T> p) {
        return  eat().bind(w1->
                p.bind(x->
                        eat().bind(w2->
                                Parser.unit(x))));
    }
}

class Int {
    //[0-9]+
    static Parser<Integer> eat() {
        return  ManyOrSome.some_(Digit.eat()).bind(s->
                Parser.unit(Integer.valueOf(s)));
    }
}

class Real {
    //  [0-9]+
    // |[0-9]+ '.' [0-9]+
    static Parser<Double> eat() {
        return  ManyOrSome.some_(Digit.eat()).bind(s1->
                Char.eat('.').bind(dot->
                        ManyOrSome.some_(Digit.eat()).bind(s2->
                                Parser.unit(Double.valueOf(s1+dot+s2))))).attempt()
                .or(
                        ManyOrSome.some_(Digit.eat()).bind(s->
                                Parser.unit(Double.valueOf(s))));
    }
}

class Strg {
    static Parser<String> eat(String s) {
        if (s.isEmpty()) {
            return  Parser.unit("");
        } else {
            return  Char.eat(s.charAt(0)).bind(c->
                    eat(s.substring(1)).bind(s_->
                            Parser.unit(c + s_)));
        }
    }
    static Parser<Unit> eatOnly(String s) {
        if (s.isEmpty()) {
            return  Parser.unit(new Unit());
        } else {
            return  Char.eat(s.charAt(0)).bind(u->eatOnly(s.substring(1)));
        }
    }
}

