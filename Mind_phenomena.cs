using System;
using System.Linq;
using System.Threading.Tasks;

class Program
{
    const int k = 2; // Dimension of mind state
    const int n = 3; // Number of phenomena archetypes
    const double alpha = 0.01; // Learning rate
    const int n_iter = 100; // Number of iterations
    static readonly Random rng = new Random(42);

    // Compute dot product
    static double DotProduct(double[] a, double[] b)
    {
        return a.Zip(b, (x, y) => x * y).Sum();
    }

    // Compute probability matrix
    static double[] ProbMatrix(double[] m, double[,] e)
    {
        double[] probs = new double[n];
        double sumExp = 0.0;

        // Parallel computation of scores
        Parallel.For(0, n, i =>
        {
            double score = DotProduct(m, Enumerable.Range(0, k).Select(j => e[i, j]).ToArray());
            probs[i] = Math.Exp(score);
            lock (probs) sumExp += probs[i];
        });

        return probs.Select(p => p / sumExp).ToArray();
    }

    // Sample phenomenon
    static double[] SamplePhenomenon(double[] probs, double[,] e)
    {
        double r = rng.NextDouble();
        double cumProb = 0.0;
        for (int i = 0; i < n; i++)
        {
            cumProb += probs[i];
            if (r <= cumProb)
            {
                return Enumerable.Range(0, k).Select(j => e[i, j]).ToArray();
            }
        }
        return Enumerable.Range(0, k).Select(j => e[n - 1, j]).ToArray();
    }

    // Update mind state
    static double[] UpdateMind(double[] m, double[] e_t)
    {
        double[] m_new = new double[k];
        Parallel.For(0, k, i => m_new[i] = m[i] + alpha * e_t[i]);
        return m_new;
    }

    // One step
    static double[] Step(double[] m, double[,] e)
    {
        double[] probs = ProbMatrix(m, e);
        double[] e_t = SamplePhenomenon(probs, e);
        return UpdateMind(m, e_t);
    }

    // Simulate
    static double[] Simulate(double[] m0, double[,] e, int n_iter)
    {
        double[] m = (double[])m0.Clone();
        for (int t = 0; t < n_iter; t++)
        {
            m = Step(m, e);
        }
        return m;
    }

    static void Main()
    {
        // Initialize
        double[] m0 = { 0.5, 0.5 }; // Initial mind state
        double[,] e = { // Phenomena archetypes
            { 1.0, 0.0 },
            { 0.0, 1.0 },
            { 1.0, 1.0 }
        };

        // Run simulation
        double[] m_final = Simulate(m0, e, n_iter);

        // Print results
        Console.WriteLine($"Initial mind state: [{string.Join(", ", m0)}]");
        Console.WriteLine($"Final mind state: [{string.Join(", ", m_final)}]");
    }
}
