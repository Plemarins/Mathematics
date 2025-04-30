using System;
using System.Collections.Generic;
using System.Linq;

// ディレクテッドエボリューションの拡張シミュレーション
// アナロジー：鍵（シーケンス）を変形して錠（ターゲット）に最適化
// 論理：点変異→適応度評価→選択を繰り返し、多様性を維持
class DirectedEvolution
{
    // パラメータ
    private const int PopulationSize = 100;       // 集団サイズ
    private const int MaxGenerations = 50;        // 最大世代数
    private const int SequenceLength = 10;        // シーケンス長
    private const double FitnessThreshold = 0.9;  // 適応度閾値
    private const double MutationRate = 1.0;      // 変異率（1シーケンスあたり1箇所）
    private static readonly char[] AminoAcids = { 'A', 'C', 'G', 'T' }; // アミノ酸
    private static readonly char[] Target = "AAAAAACCCC".ToCharArray(); // ターゲット
    private static readonly Random Random = new Random();

    // シーケンスクラス
    private class Sequence
    {
        public char[] AminoAcids { get; }
        public double Fitness { get; set; }

        public Sequence(char[] aminoAcids)
        {
            AminoAcids = aminoAcids;
        }

        // コピーコンストラクタ
        public Sequence(Sequence other) : this((char[])other.AminoAcids.Clone()) { }

        // 適応度計算：ハミング距離の逆数
        public void ComputeFitness(char[] target)
        {
            int hammingDistance = AminoAcids.Zip(target, (a, b) => a != b ? 1 : 0).Sum();
            Fitness = 1.0 / (1.0 + hammingDistance);
        }
    }

    // 多様性計算：集団内の平均ハミング距離
    private static double ComputeDiversity(List<Sequence> population)
    {
        double hammingSum = 0.0;
        double pairs = 0.0;
        for (int i = 0; i < population.Count - 1; i++)
        {
            for (int j = i + 1; j < population.Count; j++)
            {
                hammingSum += population[i].AminoAcids.Zip(population[j].AminoAcids, (a, b) => a != b ? 1 : 0).Sum();
                pairs += 1.0;
            }
        }
        return hammingSum / pairs / SequenceLength;
    }

    // ランダムシーケンス生成
    private static Sequence GenerateRandomSequence()
    {
        char[] aminoAcids = new char[SequenceLength];
        for (int i = 0; i < SequenceLength; i++)
        {
            aminoAcids[i] = AminoAcids[Random.Next(AminoAcids.Length)];
        }
        return new Sequence(aminoAcids);
    }

    // 変異：1箇所の点変異
    private static Sequence Mutate(Sequence seq)
    {
        if (Random.NextDouble() >= MutationRate) return new Sequence(seq);
        var mutated = new Sequence(seq);
        int pos = Random.Next(SequenceLength);
        mutated.AminoAcids[pos] = AminoAcids[Random.Next(AminoAcids.Length)];
        return mutated;
    }

    // メインシミュレーション
    public static void Run()
    {
        // 初期化：ランダムなシーケンス集団
        var population = Enumerable.Range(0, PopulationSize)
            .Select(_ => GenerateRandomSequence())
            .ToList();

        // 適応度履歴
        var fitnessHistory = new List<(double MaxFitness, double AvgFitness)>();

        // メインループ
        for (int gen = 1; gen <= MaxGenerations; gen++)
        {
            // 変異
            var mutants = population.Select(Mutate).ToList();

            // 評価：適応度
            foreach (var seq in mutants)
            {
                seq.ComputeFitness(Target);
            }

            // 多様性計算
            double diversity = ComputeDiversity(mutants);

            // 選択：上位50%を保持、残りはランダム生成
            var sorted = mutants.OrderByDescending(s => s.Fitness).ToList();
            population = sorted.Take(PopulationSize / 2)
                .Concat(Enumerable.Range(0, PopulationSize / 2).Select(_ => GenerateRandomSequence()))
                .ToList();

            // 適応度履歴
            double maxFitness = sorted[0].Fitness;
            double avgFitness = mutants.Average(s => s.Fitness);
            fitnessHistory.Add((maxFitness, avgFitness));

            // 結果表示
            Console.WriteLine($"Gen {gen:D3}: Best fitness = {maxFitness:F4}, Avg fitness = {avgFitness:F4}, Diversity = {diversity:F4}");
            Console.WriteLine($"Best seq = {new string(sorted[0].AminoAcids)}");

            // 終了条件
            if (maxFitness >= FitnessThreshold)
            {
                Console.WriteLine("Target fitness reached. Stopping.");
                break;
            }
        }

        // 最終結果：適応度履歴の要約
        Console.WriteLine("\nEvolution Summary:");
        Console.WriteLine("Generation | Max Fitness | Avg Fitness");
        for (int i = 0; i < fitnessHistory.Count; i++)
        {
            Console.WriteLine($"{i + 1,10} {fitnessHistory[i].MaxFitness,12:F4} {fitnessHistory[i].AvgFitness,12:F4}");
        }
    }

    // エントリーポイント
    static void Main()
    {
        Run();
    }
}
