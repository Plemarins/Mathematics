namespace CSharpCategory;

// モノイドインターフェース
public interface IMonoid<T>
{
    T Append(T other);
    static abstract T Identity { get; }
}

// IntSumモノイド実装
public struct IntSum : IMonoid<int>
{
    private readonly int _value;
    public IntSum(int value) => _value = value;

    public int Append(int other) => _value + other;
    public static int Identity => 0;

    public static implicit operator IntSum(int value) => new IntSum(value);
    public static implicit operator int(IntSum sum) => sum._value;
}

// ListMonoidモノイド実装
public struct ListMonoid<T> : IMonoid<List<T>>
{
    private readonly List<T> _value;
    public ListMonoid(List<T> value) => _value = value;

    public List<T> Append(List<T> other) => _value.Concat(other).ToList();
    public static List<T> Identity => new List<T>();

    public static implicit operator ListMonoid<T>(List<T> value) => new ListMonoid<T>(value);
    public static implicit operator List<T>(ListMonoid<T> monoid) => monoid._value;
}

// モノイドの使用例
public static class MonoidExample
{
    public static void Run()
    {
        IntSum sum1 = 5;
        IntSum sum2 = 10;
        Console.WriteLine(sum1.Append(sum2)); // 出力: 15

        ListMonoid<int> list1 = new List<int> { 1, 2 };
        ListMonoid<int> list2 = new List<int> { 3, 4 };
        var combined = list1.Append(list2);
        Console.WriteLine(string.Join(", ", combined)); // 出力: 1, 2, 3, 4
    }
}
