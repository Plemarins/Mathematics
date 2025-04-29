using System;
using System.Collections.Generic;
using System.Linq;

namespace ResourceEvolutionModel
{
    // 状態（戦略分布とリソース状態）
    public class State
    {
        public string Id { get; }
        public double[] StrategyDistribution { get; set; } // 戦略分布 x
        public double Resource { get; set; } // リソース量
        public double Fitness { get; set; } // 平均適応度
        public State(string id, double[] strategyDistribution, double resource)
        {
            Id = id;
            StrategyDistribution = strategyDistribution;
            Resource = resource;
            Fitness = 0.0;
        }
    }

    // 戦略
    public class Strategy
    {
        public string Name { get; }
        public double Cost { get; } // 戦略のコスト
        public Strategy(string name, double cost)
        {
            Name = name;
            Cost = cost;
        }
    }

    // アクセシビリティ関係（圏論的射 + 確率）
    public class Transition
    {
        public State Source { get; }
        public State Target { get; }
        public Strategy Strategy { get; }
        public double Probability { get; } // 遷移確率
        public Transition(State source, State target, Strategy strategy, double probability)
        {
            Source = source;
            Target = target;
            Strategy = strategy;
            Probability = probability;
        }
    }

    // 進化ゲームモデル
    public class ResourceEvolutionModel
    {
        private List<State> States { get; } // 状態空間
        private List<Strategy> Strategies { get; } // 戦略空間
        private List<Transition> Transitions { get; } // アクセシビリティ関係
        private readonly Random random;
        private readonly double dt = 0.01; // 時間ステップ
        private readonly double resourceDecay = 0.1; // リソース消費率

        public ResourceEvolutionModel()
        {
            States = new List<State>();
            Strategies = new List<Strategy>();
            Transitions = new List<Transition>();
            random = new Random();
        }

        // 状態の追加
        public void AddState(string id, double[] strategyDistribution, double resource)
        {
            States.Add(new State(id, strategyDistribution, resource));
        }

        // 戦略の追加
        public void AddStrategy(string name, double cost)
        {
            Strategies.Add(new Strategy(name, cost));
        }

        // アクセシビリティ関係の追加
        public void AddTransition(string sourceId, string targetId, string strategyName, double probability)
        {
            var source = States.Find(s => s.Id == sourceId);
            var target = States.Find(s => s.Id == targetId);
            var strategy = Strategies.Find(s => s.Name == strategyName);
            if (source != null && target != null && strategy != null)
            {
                Transitions.Add(new Transition(source, target, strategy, probability));
            }
        }

        // 適応度計算
        private double ComputeFitness(Strategy strategy, State state)
        {
            // 適応度はリソースに比例、コストに反比例
            return state.Resource / (1.0 + strategy.Cost);
        }

        // レプリケーターダイナミクス
        private void UpdateStrategyDistribution(State state)
        {
            double[] x = state.StrategyDistribution;
            double[] fitness = new double[Strategies.Count];
            double avgFitness = 0.0;

            // 各戦略の適応度
            for (int i = 0; i < Strategies.Count; i++)
            {
                fitness[i] = ComputeFitness(Strategies[i], state);
                avgFitness += x[i] * fitness[i];
            }

            // レプリケーターダイナミクス
            double[] newX = new double[x.Length];
            double sum = 0.0;
            for (int i = 0; i < x.Length; i++)
            {
                newX[i] = x[i] * (fitness[i] - avgFitness) * dt + x[i];
                if (newX[i] < 0) newX[i] = 0;
                sum += newX[i];
            }

            // 正規化
            for (int i = 0; i < x.Length; i++)
            {
                newX[i] /= sum;
            }

            state.StrategyDistribution = newX;
            state.Fitness = avgFitness;
        }

        // モーダル論理：高適応度状態への到達可能性（S4）
        public bool CanReachHighFitness(State state, double fitnessThreshold, int maxSteps = 100)
        {
            var visited = new HashSet<State>();
            var queue = new Queue<State>();
            queue.Enqueue(state);
            visited.Add(state);

            if (state.Fitness >= fitnessThreshold)
                return true;

            while (queue.Count > 0 && maxSteps-- > 0)
            {
                var current = queue.Dequeue();
                var accessible = Transitions.Where(t => t.Source == current);
                foreach (var transition in accessible)
                {
                    var next = transition.Target;
                    if (next.Fitness >= fitnessThreshold)
                        return true;
                    if (!visited.Contains(next))
                    {
                        queue.Enqueue(next);
                        visited.Add(next);
                    }
                }
            }
            return false;
        }

        // 進化シミュレーション
        public (double, double) SimulateEvolution(State initial, int steps)
        {
            var current = initial;
            double finalFitness = current.Fitness;
            double finalResource = current.Resource;

            for (int t = 0; t < steps; t++)
            {
                var transitions = Transitions.Where(t => t.Source == current).ToList();
                if (!transitions.Any())
                    break;

                // リソース消費
                finalResource -= resourceDecay * transitions.Average(t => t.Strategy.Cost);
                if (finalResource < 0) finalResource = 0;
                current.Resource = finalResource;

                // 戦略分布更新
                UpdateStrategyDistribution(current);

                // 状態遷移
                double totalProb = transitions.Sum(t => t.Probability);
                double r = random.NextDouble() * totalProb;
                double sum = 0;
                foreach (var t in transitions)
                {
                    sum += t.Probability;
                    if (r <= sum)
                    {
                        current = t.Target;
                        break;
                    }
                }

                finalFitness = current.Fitness;
                if (finalFitness >= 1.0) // 高適応度閾値
                    break;
            }

            return (finalFitness, finalResource);
        }

        // モーダル論理評価：\diamond Fitness(s^*, r')
        public bool EvaluateProposition(State initialState, double fitnessThreshold = 1.0)
        {
            // モーダル論理：高適応度状態への到達可能性
            bool isHighFitnessPossible = CanReachHighFitness(initialState, fitnessThreshold);
            if (!isHighFitnessPossible)
                return false;

            // 確率論的評価
            var (finalFitness, finalResource) = SimulateEvolution(initialState, 1000);
            return finalFitness >= fitnessThreshold && finalResource >= 0;
        }

        // シミュレーション実行
        public void RunSimulation()
        {
            foreach (var state in States)
            {
                bool result = EvaluateProposition(state);
                Console.WriteLine($"State {state.Id} (Resource={state.Resource}): {(result ? "Evolves to high fitness" : "Does not evolve to high fitness")}");
            }
        }
    }

    class Program
    {
        static void Main()
        {
            var model = new ResourceEvolutionModel();

            // 戦略の定義
            model.AddStrategy("Cooperative", 0.3); // 協力：低コスト
            model.AddStrategy("Defective", 0.1); // 裏切り：低コスト、高適応度

            // 状態の定義
            model.AddState("s1", new double[] { 0.5, 0.5 }, 10.0); // 初期分布、リソース豊富
            model.AddState("s2", new double[] { 0.8, 0.2 }, 5.0); // 協力優勢、リソース中
            model.AddState("s3", new double[] { 0.2, 0.8 }, 1.0); // 裏切り優勢、リソース希少

            // アクセシビリティ関係の定義
            model.AddTransition("s1", "s2", "Cooperative", 0.6);
            model.AddTransition("s1", "s3", "Defective", 0.4);
            model.AddTransition("s2", "s3", "Defective", 0.5);
            model.AddTransition("s2", "s2", "Cooperative", 0.5);
            model.AddTransition("s3", "s3", "Defective", 0.8);
            model.AddTransition("s3", "s2", "Cooperative", 0.2);

            // シミュレーション実行
            model.RunSimulation();
        }
    }
}
