namespace CSharpCategory;

// ファンクターインターフェース（モナドに必要）
public interface IFunctor<T>
{
    IFunctor<U> Map<U>(Func<T, U> f);
}

// モナドインターフェース
public interface IMonad<T> : IFunctor<T>
{
    IMonad<U> Bind<U>(Func<T, IMonad<U>> f);
    IMonad<T> Pure(T value);
}

// Optionモナド実装
public abstract class Option<T> : IMonad<T>
{
    public static Option<T> Some(T value) => new Some<T>(value);
    public static Option<T> None() => new None<T>();

    public abstract IFunctor<U> Map<U>(Func<T, U> f);
    public abstract IMonad<U> Bind<U>(Func<T, IMonad<U>> f);
    public IMonad<T> Pure(T value) => Some(value);
}

public class Some<T> : Option<T>
{
    private readonly T _value;
    public Some(T value) => _value = value;

    public override IFunctor<U> Map<U>(Func<T, U> f) => Some(f(_value));
    public override IMonad<U> Bind<U>(Func<T, IMonad<U>> f) => f(_value);
}

public class None<T> : Option<T>
{
    public override IFunctor<U> Map<U>(Func<T, U> f) => None<U>();
    public override IMonad<U> Bind<U>(Func<T, IMonad<U>> f) => None<U>();
}

// Eitherモナド実装
public abstract class Either<L, R> : IMonad<R>
{
    public static Either<L, R> Right(R value) => new Right<L, R>(value);
    public static Either<L, R> Left(L value) => new Left<L, R>(value);

    public abstract IFunctor<U> Map<U>(Func<R, U> f);
    public abstract IMonad<U> Bind<U>(Func<R, IMonad<U>> f);
    public IMonad<R> Pure(R value) => Right(value);
}

public class Right<L, R> : Either<L, R>
{
    private readonly R _value;
    public Right(R value) => _value = value;

    public override IFunctor<U> Map<U>(Func<R, U> f) => Right<L, U>(f(_value));
    public override IMonad<U> Bind<U>(Func<R, IMonad<U>> f) => f(_value);
}

public class Left<L, R> : Either<L, R>
{
    private readonly L _value;
    public Left(L value) => _value = value;

    public override IFunctor<U> Map<U>(Func<R, U> f) => Left<L, U>(_value);
    public override IMonad<U> Bind<U>(Func<R, IMonad<U>> f) => Left<L, U>(_value);
}

// LINQ統合
public static class MonadExtensions
{
    public static IMonad<U> Select<T, U>(this IMonad<T> monad, Func<T, U> f) => monad.Map(f);

    public static IMonad<V> SelectMany<T, U, V>(
        this IMonad<T> monad,
        Func<T, IMonad<U>> bind,
        Func<T, U, V> project) =>
        monad.Bind(t => bind(t).Map(u => project(t, u)));
}

// モナドの使用例
public static class MonadExample
{
    public static void Run()
    {
        Option<int> input = Option<int>.Some(5);
        Either<string, int> result = input.Match(
            Some: x => x > 0 ? Either<string, int>.Right(x * 2) : Either<string, int>.Left("Invalid input"),
            None: () => Either<string, int>.Left("No input"));
        Console.WriteLine(result); // 出力: Right(10)

        var query = from x in Option<int>.Some(5)
                    from y in Option<int>.Some(10)
                    select x + y;
        Console.WriteLine(query); // 出力: Some(15)
    }
}
