using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

public class CellAgent
{
    public double MarkerLevel { get; set; } // 細胞表面マーカー
    public List<double> Genome { get; set; } // 遺伝子型
    public bool NucleaseResistance { get; set; } // ヌクレアーゼ耐性
    public double Fitness { get; set; } // 適応度

    public CellAgent(Random rand, int numLoci)
    {
        MarkerLevel = rand.NextDouble();
        Genome = Enumerable.Range(0, numLoci).Select(_ => rand.NextDouble()).ToList();
        NucleaseResistance = rand.NextDouble() < 0.1; // 10%が耐性
        CalculateFitness();
    }

    public void EditGenome(double editProb, Random rand)
    {
        if (!NucleaseResistance && rand.NextDouble() < editProb)
        {
            Genome[0] = 1.0; // 編集成功
        }
        CalculateFitness();
    }

    private void CalculateFitness()
    {
        Fitness = Genome.Average(); // マルチローカス適応度
    }
}

public class Simulation
{
    private List<CellAgent> population;
    private Random rand = new Random();

    public Simulation(int numCells, int numLoci)
    {
        population = Enumerable.Range(0, numCells)
            .Select(_ => new CellAgent(rand, numLoci))
            .ToList();
    }

    public async Task RunSimulationAsync(int generations)
    {
        for (int gen = 0; gen < generations; gen++)
        {
            // 並列処理で遺伝子編集
            await Task.WhenAll(population.Select(cell => Task.Run(() =>
            {
                cell.EditGenome(0.8, rand); // CRISPR/TALEN編集
            })));

            // 進化（選択）
            var sorted = population.OrderByDescending(c => c.Fitness).Take(population.Count / 2).ToList();
            population = sorted.Select(c => new CellAgent(rand, c.Genome.Count)).ToList();

            // 遺伝的多様性計算
            double diversity = population.SelectMany(c => c.Genome).Distinct().Count();
            Console.WriteLine($"Generation {gen}: Diversity = {diversity}");
        }
    }
}

class Program
{
    static async Task Main()
    {
        var sim = new Simulation(1000, 10);
        await sim.RunSimulationAsync(100);
    }
}
