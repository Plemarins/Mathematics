using System;
using System.Linq;

class StabilityAnalysis
{
    const int n = 6; // 状態数
    static double[,] P = new double[n, n]; // 確率行列
    static double[] pi; // 確率分布
    static double[] systemVals = { 1.0, 1.0, 1.0, 0.0, 0.0, 0.0 }; // system: Up=1, Down=0
    static double[] stressVals = { 0.0, 1.0, 2.0, 0.0, 1.0, 2.0 }; // stress: Low=0, Medium=1, High=2

    static void Main()
    {
        // 確率行列Pの初期化
        InitializeMatrix();
        // 初期分布piの設定
        pi = new double[] { 0.5, 0.3, 0.1, 0.05, 0.03, 0.02 };
        int steps = 10;
        double[] covHistory = new double[steps + 1];
        covHistory[0] = ComputeCovariance(pi);

        // 1. 変換：時間発展
        Console.WriteLine("Time evolution of probability distribution:");
        double[] piNext = (double[])pi.Clone();
        for (int t = 1; t <= steps; t++)
        {
            piNext = MatrixVectorMult(piNext);
            Console.WriteLine($"t={t}: [{string.Join(", ", piNext.Select(x => x.ToString("F4")))}]");
            covHistory[t] = ComputeCovariance(piNext);
        }

        // 2. 不変性：定常分布
        double[] piStar = StationaryDistribution();
        Console.WriteLine("\nStationary distribution:");
        Console.WriteLine($"[{string.Join(", ", piStar.Select(x => x.ToString("F4")))}]");

        // 3. 対称性：ストレス遷移の対称性チェック
        bool symmetry = Math.Abs(P[0, 1] - P[3, 4]) < 1e-5; // (Up,Low)->(Up,Medium) vs (Down,Low)->(Down,Medium)
        Console.WriteLine($"\nSymmetry in stress transition: {symmetry}");

        // 4. 相互作用：システムDown時のストレスHigh確率
        double pStressHighGivenDown = Enumerable.Range(0, n).Sum(i => P[i, 5]) /
                                      Enumerable.Range(0, n).Sum(i => P[i, 3] + P[i, 4] + P[i, 5]);
        Console.WriteLine($"P(stress=High | system=Down): {pStressHighGivenDown:F4}");

        // 5. 共変性：共分散の履歴
        Console.WriteLine("\nCovariance over time:");
        for (int t = 0; t <= steps; t++)
        {
            Console.WriteLine($"t={t}: {covHistory[t]:F4}");
        }
    }

    // 確率行列Pの初期化
    static void InitializeMatrix()
    {
        P = new double[,]
        {
            { 0.7, 0.2, 0.05, 0.03, 0.01, 0.01 }, // (Up,Low)
            { 0.1, 0.6, 0.2, 0.05, 0.03, 0.02 },  // (Up,Medium)
            { 0.05, 0.1, 0.7, 0.05, 0.05, 0.05 }, // (Up,High)
            { 0.1, 0.05, 0.05, 0.6, 0.1, 0.1 },   // (Down,Low)
            { 0.05, 0.1, 0.05, 0.1, 0.6, 0.1 },   // (Down,Medium)
            { 0.01, 0.01, 0.1, 0.1, 0.1, 0.68 }   // (Down,High)
        };
    }

    // 行列ベクトル積：pi_new = pi ・ P
    static double[] MatrixVectorMult(double[] pi)
    {
        double[] piNew = new double[n];
        for (int i = 0; i < n; i++)
        {
            for (int j = 0; j < n; j++)
            {
                piNew[i] += pi[j] * P[j, i];
            }
        }
        return piNew;
    }

    // 定常分布の計算（簡略化：反復で近似）
    static double[] StationaryDistribution()
    {
        double[] piStar = Enumerable.Repeat(1.0 / n, n).ToArray();
        for (int i = 0; i < 100; i++) // 反復回数
        {
            piStar = MatrixVectorMult(piStar);
            double sum = piStar.Sum();
            piStar = piStar.Select(x => x / sum).ToArray(); // 正規化
        }
        return piStar;
    }

    // 共分散の計算
    static double ComputeCovariance(double[] pi)
    {
        double E_system = 0, E_stress = 0, E_system_stress = 0;
        for (int i = 0; i < n; i++)
        {
            E_system += pi[i] * systemVals[i];
            E_stress += pi[i] * stressVals[i];
            E_system_stress += pi[i] * systemVals[i] * stressVals[i];
        }
        return E_system_stress - E_system * E_stress;
    }
}
