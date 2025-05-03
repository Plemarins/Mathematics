namespace CSharpCategory;

// ファンクターインターフェース（コモナドに必要）
public interface IFunctor<T>
{
    IFunctor<U> Map<U>(Func<T, U> f);
}

// コモナドインターフェース
public interface IComonad<T> : IFunctor<T>
{
    T Extract();
    IComonad<U> Extend<U>(Func<IComonad<T>, U> f);
}

// Storeコモナド実装
public class Store<S, A> : IComonad<A>
{
    public S State { get; }
    public Func<S, A> Lookup { get; }

    public Store(S state, Func<S, A> lookup) => (State, Lookup) = (state, lookup);

    public A Extract() => Lookup(State);

    public IFunctor<B> Map<B>(Func<A, B> f) =>
        new Store<S, B>(State, s => f(Lookup(s)));

    public IComonad<B> Extend<B>(Func<IComonad<A>, B> f) =>
        new Store<S, B>(State, s => f(new Store<S, A>(s, Lookup)));
}

// コモナドの使用例
public static class ComonadExample
{
    public static void Run()
    {
        var store = new Store<int, string>(42, x => $"Value: {x}");
        var extended = store.Extend(s => s.Extract() + " Extended");
        Console.WriteLine(extended.Extract()); // 出力: "Value: 42 Extended"
    }
}
