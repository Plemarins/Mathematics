namespace CSharpCategory;

// ファンクターインターフェース（ファイブレーションに必要）
public interface IFunctor<T>
{
    IFunctor<U> Map<U>(Func<T, U> f);
}

// Optionモナド（ファイブレーションの例で使用）
public abstract class Option<T> : IFunctor<T>
{
    public static Option<T> Some(T value) => new Some<T>(value);
    public static Option<T> None() => new None<T>();

    public abstract IFunctor<U> Map<U>(Func<T, U> f);
}

public class Some<T> : Option<T>
{
    private readonly T _value;
    public Some(T value) => _value = value;

    public override IFunctor<U> Map<U>(Func<T, U> f) => Some(f(_value));
}

public class None<T> : Option<T>
{
    public override IFunctor<U> Map<U>(Func<T, U> f) => None<U>();
}

// ファイブレーション実装
public class Fibration<F, A> where F : IFunctor<A>
{
    public F BaseFunctor { get; }
    public Func<A, F> Reindex { get; }

    public Fibration(F baseFunctor, Func<A, F> reindex)
    {
        BaseFunctor = baseFunctor;
        Reindex = reindex;
    }

    public Fibration<F, B> Map<B>(Func<A, B> f) =>
        new Fibration<F, B>(BaseFunctor, x => Reindex(f(x)));
}

// ファイブレーションの使用例
public static class FibrationExample
{
    public static void Run()
    {
        var optionFibration = new Fibration<Option<int>, int>(Option<int>.Some(10), x => Option<int>.Some(x + 1));
        var mappedFibration = optionFibration.Map(x => x * 2);
        Console.WriteLine(mappedFibration.Reindex(5)); // 出力: Some(11)
    }
}
