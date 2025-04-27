using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace NeuromorphicCiM
{
    public class NeuromorphicCiM
    {
        private const int MaxTime = 10; // 最大時間ステップ
        private const int NumNeurons = 2; // ニューロンの数（A, B）
        private const double Threshold = 0.8; // シナプス重みの閾値
        private const double LearningRate = 0.1; // STDPの学習率

        private int[] SpikeA; // ニューロンAのスパイク列
        private int[] SpikeB; // ニューロンBのスパイク列
        private double SynapseWeight; // シナプス重み（A → B）
        private int[,] States; // Kripke構造（状態遷移）
        private List<string> Results; // 結果ログ

        public NeuromorphicCiM()
        {
            Initialize();
        }

        // 初期化
        private void Initialize()
        {
            SpikeA = new int[MaxTime];
            SpikeB = new int[MaxTime];
            States = new int[MaxTime, 2]; // 2状態モデル
            Results = new List<string>();
            SynapseWeight = 0.5;

            // サンプルスパイク列とKripke構造
            for (int t = 0; t < MaxTime; t++)
            {
                SpikeA[t] = t % 2 == 0 ? 1 : 0; // A: 偶数時刻でスパイク
                SpikeB[t] = t % 3 == 0 ? 1 : 0; // B: 3の倍数時刻でスパイク
                States[t, 0] = 1; // 状態0は常に有効
                States[t, 1] = t > 5 ? 1 : 0; // 状態1はt>5で有効
            }
        }

        // スパイクベースの論理演算（A ∧ B）
        private int ComputeAND(int spike1, int spike2)
        {
            return (spike1 == 1 && spike2 == 1) ? 1 : 0;
        }

        // 直観主義的演算（A → B）
        private bool ComputeIMPLY(int spike1, int spike2, double weight)
        {
            if (spike1 == 1)
            {
                return spike2 == 1 && weight >= Threshold;
            }
            return true; // Aが偽ならA → Bは真（直観主義）
        }

        // シナプス重みの更新（STDP）
        private void UpdateSynapseWeight(int spike1, int spike2)
        {
            if (spike1 == 1 && spike2 == 1)
            {
                // Aの後にBが発火する場合、重み増加
                SynapseWeight += LearningRate * (1 - SynapseWeight);
            }
            else if (spike1 == 1 && spike2 == 0)
            {
                // Aが発火しBが発火しない場合、重み減少
                SynapseWeight -= LearningRate * SynapseWeight;
            }

            // 重みの範囲を制限
            SynapseWeight = Math.Max(0, Math.Min(1, SynapseWeight));
        }

        // 時相論理の評価（◇(A ∧ B)）
        private bool EvaluateTemporalLogic()
        {
            for (int t = 0; t < MaxTime; t++)
            {
                if (States[t, 0] == 1 && ComputeAND(SpikeA[t], SpikeB[t]) == 1)
                {
                    return true;
                }
            }
            return false;
        }

        // ファイブレーションシミュレーション
        private void SimulateFibration()
        {
            Results.Add("Time,SpikeA,SpikeB,A ∧ B,A → B,SynapseWeight");

            for (int t = 0; t < MaxTime; t++)
            {
                // 論理演算（ファイバー圏）
                int andResult = ComputeAND(SpikeA[t], SpikeB[t]);
                bool implyResult = ComputeIMPLY(SpikeA[t], SpikeB[t], SynapseWeight);

                // シナプス重みの更新（STDP）
                UpdateSynapseWeight(SpikeA[t], SpikeB[t]);

                // 結果の記録
                Results.Add($"{t},{SpikeA[t]},{SpikeB[t]},{andResult},{implyResult},{SynapseWeight:F2}");
            }

            // 時相論理の結果
            Results.Add($"Temporal Logic (◇(A ∧ B)): {EvaluateTemporalLogic()}");
        }

        // CSVに出力
        private void OutputToCSV()
        {
            File.WriteAllLines("SimulationResults.csv", Results);
            Console.WriteLine("Simulation completed! Results saved to SimulationResults.csv");
        }

        // メイン処理
        public void RunSimulation()
        {
            SimulateFibration();
            OutputToCSV();
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var simulator = new NeuromorphicCiM();
            simulator.RunSimulation();
        }
    }
}
