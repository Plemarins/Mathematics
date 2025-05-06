using System;
using System.Numerics;
using System.Collections.Generic;
using System.Linq;

// 複素数対応ラプラス変換
public class LaplaceTransform
{
    public Func<Complex, Complex> F { get; }

    public LaplaceTransform(Func<Complex, Complex> f)
    {
        F = f;
    }

    // 初期値の定理: lim_{s->∞} s*F(s)
    public Complex InitialValue()
    {
        var largeS = new Complex(1e6, 0);
        return largeS * F(largeS);
    }

    // 最終値の定理: lim_{s->0} s*F(s)
    public Complex FinalValue()
    {
        var smallS = new Complex(1e-6, 0);
        return smallS * F(smallS);
    }

    // 安定性解析: 極の位置を確認
    public bool IsStable(IEnumerable<Complex> poles)
    {
        return poles.All(p => p.Real < 0);
    }
}

// 回路網（グラフモデル）
public class Circuit
{
    private readonly List<(Complex Voltage, Complex Current)> _branches;
    private readonly List<(int From, int To)> _edges; // グラフの辺

    public Circuit(List<(Complex Voltage, Complex Current)> branches, List<(int From, int To)> edges)
    {
        _branches = branches;
        _edges = edges;
    }

    // テレゲンの定理: Σ v_k * i_k = 0
    public Complex TellegenSum()
    {
        return _branches.Aggregate(Complex.Zero, (sum, branch) => sum + branch.Voltage * branch.Current);
    }

    public bool VerifyTellegen(double tolerance = 1e-10)
    {
        return Complex.Abs(TellegenSum()) < tolerance;
    }

    // グラフの隣接行列
    public int[,] AdjacencyMatrix(int nodeCount)
    {
        var matrix = new int[nodeCount, nodeCount];
        foreach (var edge in _edges)
        {
            matrix[edge.From, edge.To] = 1;
            matrix[edge.To, edge.From] = 1; // 無向グラフ
        }
        return matrix;
    }
}

// 圏論的インターフェース
public interface ICategory
{
    Complex Evaluate(object obj, Complex param);
}

public class LaplaceCategory : ICategory
{
    public Complex Evaluate(object obj, Complex param)
    {
        if (obj is LaplaceTransform lt)
            return param * lt.F(param);
        throw new InvalidOperationException("Invalid object.");
    }
}

class Program
{
    static void Main()
    {
        // ラプラス変換: F(s) = 1/(s^2 + 2s + 1)
        var laplace = new LaplaceTransform(s => 1.0 / (s * s + 2.0 * s + 1.0));
        Console.WriteLine($"Initial Value: {laplace.InitialValue()}");
        Console.WriteLine($"Final Value: {laplace.FinalValue()}");

        // 安定性解析
        var poles = new List<Complex> { new Complex(-1, 0), new Complex(-1, 0) };
        Console.WriteLine($"System Stable: {laplace.IsStable(poles)}");

        // テレゲンの定理
        var circuit = new Circuit(
            branches: new List<(Complex, Complex)>
            {
                (new Complex(2.0, 0), new Complex(3.0, 0)),
                (new Complex(-2.0, 0), new Complex(-3.0, 0))
            },
            edges: new List<(int, int)> { (0, 1), (1, 0) }
        );
        Console.WriteLine($"Tellegen Sum: {circuit.TellegenSum()}");
        Console.WriteLine($"Tellegen Verified: {circuit.VerifyTellegen()}");

        // 隣接行列
        var adjMatrix = circuit.AdjacencyMatrix(2);
        Console.WriteLine("Adjacency Matrix:");
        for (int i = 0; i < 2; i++)
        {
            for (int j = 0; j < 2; j++)
                Console.Write($"{adjMatrix[i, j]} ");
            Console.WriteLine();
        }

        // 圏論的評価
        var category = new LaplaceCategory();
        Console.WriteLine($"Category Eval at s=1e6: {category.Evaluate(laplace, new Complex(1e6, 0))}");
    }
}
