# Plemarins Andrapaat's creation.

using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using MathNet.Numerics.LinearAlgebra;
using MathNet.Numerics.LinearAlgebra.Complex;
using MathNet.Numerics.Random;

class HappinessSimulation
{
    // パラメータ
    private const double T = 100.0, dt = 0.1, sigma = 0.1, Delta = 0.5;
    private const double L = 10.0, dx = 0.1, k0 = 1.0, k1 = 0.1;
    private const double omega = 0.1, k_wave = 0.2, noise_sigma = 0.1;
    private const double sigma_q = 0.0173, kappa = 0.1, g = 0.1, hbar = 1.0, lambda = 0.5;
    private static readonly double[] w = { 0.4, 0.3, 0.2 };
    private const int n_steps = (int)(T / dt) + 1, n_dim = 3, n_x = (int)(L / dx) + 1;

    // 仮定データ
    private const double mu_E = 3.0, mu_S = 10.0, mu_C = 5.0;
    private const double sigma_E = 0.707, sigma_S = 1.414, sigma_C = 1.0;
    private static readonly double[] a = { sigma_E, sigma_S, sigma_C };
    private static readonly double[] mu = { mu_E, mu_S, mu_C };

    // 乱数生成
    private static readonly Random rand = new Random();
    private static double GaussianRandom()
    {
        double u1 = rand.NextDouble(), u2 = rand.NextDouble();
        return Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Cos(2.0 * Math.PI * u2);
    }

    static void Main()
    {
        // VEV計算
        double sum_mu = mu_E + mu_S + mu_C;
        double[] p = { mu_E / sum_mu, mu_S / sum_mu, mu_C / sum_mu };
        double vev = a.Zip(p, (ai, pi) => ai * pi).Sum();
        Console.WriteLine($"Vacuum Expectation Value of A_mu: {vev}");

        // データ生成
        double[,] data_E = new double[n_steps, n_x], data_S = new double[n_steps, n_x], data_C = new double[n_steps, n_x];
        double[,] A0_E = new double[n_steps, n_x], A0_S = new double[n_steps, n_x], A0_C = new double[n_steps, n_x];
        double[,] A1_E = new double[n_steps, n_x], A1_S = new double[n_steps, n_x], A1_C = new double[n_steps, n_x];
        double[,,,] xi_mu = new double[n_steps, n_x, 2, n_dim]; // 量子ノイズ
        double[,] xi_F = new double[n_steps, n_x]; // 場強度ノイズ

        for (int i = 0; i < n_steps; i++)
        {
            double t = i * dt;
            for (int ix = 0; ix < n_x; ix++)
            {
                double x = ix * dx;
                data_E[i, ix] = mu_E + sigma_E * Math.Sin(omega * t + k_wave * x) + noise_sigma * GaussianRandom();
                data_S[i, ix] = mu_S + sigma_S * Math.Sin(omega * t + k_wave * x) + noise_sigma * GaussianRandom();
                data_C[i, ix] = mu_C + sigma_C * Math.Sin(omega * t + k_wave * x) + noise_sigma * GaussianRandom();
                xi_F[i, ix] = GaussianRandom();
                for (int j = 0; j < 2; j++)
                    for (int k = 0; k < n_dim; k++)
                        xi_mu[i, ix, j, k] = sigma_q * GaussianRandom();
            }
        }

        // ゲージ場 A_mu
        for (int i = 0; i < n_steps; i++)
        {
            for (int ix = 0; ix < n_x; ix++)
            {
                A0_E[i, ix] = k0 * (data_E[i, ix] - mu_E) + xi_mu[i, ix, 0, 0];
                A0_S[i, ix] = k0 * (data_S[i, ix] - mu_S) + xi_mu[i, ix, 0, 1];
                A0_C[i, ix] = k0 * (data_C[i, ix] - mu_C) + xi_mu[i, ix, 0, 2];
                if (ix < n_x - 1)
                {
                    A1_E[i, ix] = k1 * (data_E[i, ix + 1] - data_E[i, ix]) / dx + xi_mu[i, ix, 1, 0];
                    A1_S[i, ix] = k1 * (data_S[i, ix + 1] - data_S[i, ix]) / dx + xi_mu[i, ix, 1, 1];
                    A1_C[i, ix] = k1 * (data_C[i, ix + 1] - data_C[i, ix]) / dx + xi_mu[i, ix, 1, 2];
                }
            }
        }

        // 場強度 F_01
        double[] F_01 = new double[n_steps], F_01_big = new double[n_steps];
        for (int i = 0; i < n_steps - 1; i++)
        {
            for (int ix = 0; ix < n_x - 1; ix++)
            {
                F_01[i] += ((
                    (A1_E[i + 1, ix] - A1_E[i, ix]) / dt - (A0_E[i, ix + 1] - A0_E[i, ix]) / dx +
                    (A1_S[i + 1, ix] - A1_S[i, ix]) / dt - (A0_S[i, ix + 1] - A0_S[i, ix]) / dx +
                    (A1_C[i + 1, ix] - A1_C[i, ix]) / dt - (A0_C[i, ix + 1] - A0_C[i, ix]) / dx
                ) / (n_x - 1)) + kappa * sigma_q * sigma_q * xi_F[i, ix];
                F_01_big[i] += ((
                    (A1_E[i + 1, ix] - A1_E[i, ix]) / dt - (A0_E[i, ix + 1] - A0_E[i, ix] + Delta) / dx +
                    (A1_S[i + 1, ix] - A1_S[i, ix]) / dt - (A0_S[i, ix + 1] - A0_S[i, ix]) / dx +
                    (A1_C[i + 1, ix] - A1_C[i, ix]) / dt - (A0_C[i, ix + 1] - A0_C[i, ix]) / dx
                ) / (n_x - 1)) + kappa * sigma_q * sigma_q * xi_F[i, ix];
            }
        }

        Console.WriteLine($"Average F_01 (No BIG): {F_01.Average()}");
        Console.WriteLine($"Average F_01 (With BIG): {F_01_big.Average()}");

        // シミュレーション
        double[] H_total = new double[n_steps], H_total_big = new double[n_steps];
        double[] A_expect = new double[n_steps], A_expect_big = new double[n_steps];

        Parallel.For(0, 4, _ =>
        {
            var x = Vector<double>.Build.Dense(n_dim, 0.0);
            var x_big = Vector<double>.Build.Dense(n_dim, 0.0);
            var psi = Vector<MathNet.Numerics.Complex>.Build.Dense(n_dim, new MathNet.Numerics.Complex(1.0 / Math.Sqrt(3.0), 0.0));
            var psi_big = psi.Copy();
            var A_mu = Vector<double>.Build.Dense(n_dim, 0.0);
            var b = Vector<double>.Build.Dense(new[] { Delta, 0.0, 0.0 });
            var H0 = Matrix<MathNet.Numerics.Complex>.Build.DenseDiagonal(n_dim, n_dim, i => new MathNet.Numerics.Complex(w[i], 0.0));
            var H_BIG = Matrix<MathNet.Numerics.Complex>.Build.DenseDiagonal(n_dim, n_dim, (i, j) => i == 0 && j == 0 ? new MathNet.Numerics.Complex(Delta, 0.0) : 0.0);
            var A_op = Matrix<MathNet.Numerics.Complex>.Build.DenseDiagonal(n_dim, n_dim, i => new MathNet.Numerics.Complex(a[i], 0.0));

            var H_classical = new double[n_steps];
            var H_classical_big = new double[n_steps];
            var H_quantum = new double[n_steps];
            var H_quantum_big = new double[n_steps];

            for (int i = 0; i < n_steps; i++)
            {
                x = UpdateClassical(x, A_mu, b, F_01[i], false);
                x_big = UpdateClassical(x_big, A_mu, b, F_01_big[i], true);
                psi = UpdateQuantum(psi, H0, H_BIG, false);
                psi_big = UpdateQuantum(psi_big, H0, H_BIG, true);

                H_classical[i] = HappinessClassical(x);
                H_classical_big[i] = HappinessClassical(x_big);
                H_quantum[i] = HappinessQuantum(psi, H0).Real;
                H_quantum_big[i] = HappinessQuantum(psi_big, H0).Real;
                H_total[i] = lambda * H_classical[i] + (1.0 - lambda) * H_quantum[i];
                H_total_big[i] = lambda * H_classical_big[i] + (1.0 - lambda) * H_quantum_big[i];
                A_expect[i] = ComputeAExpect(psi, A_op).Real;
                A_expect_big[i] = ComputeAExpect(psi_big, A_op).Real;
            }

            lock (H_total)
            {
                for (int i = 0; i < n_steps; i++)
                {
                    H_total[i] = H_total[i];
                    H_total_big[i] = H_total_big[i];
                    A_expect[i] = A_expect[i];
                    A_expect_big[i] = A_expect_big[i];
                }
            }
        });

        // 結果出力
        using (var writer = new StreamWriter("happiness_gauge_simulation.csv"))
        {
            writer.WriteLine("Time,H_total,H_total_big,A_expect,A_expect_big,F_01,F_01_big");
            for (int i = 0; i < n_steps; i++)
            {
                writer.WriteLine($"{i * dt},{H_total[i]},{H_total_big[i]},{A_expect[i]},{A_expect_big[i]},{F_01[i]},{F_01_big[i]}");
            }
        }
    }

    static Matrix<double> ComputeA(Vector<double> x, Vector<double> A_mu, double F_01)
    {
        var A_mat = Matrix<double>.Build.Dense(n_dim, n_dim);
        var F = Matrix<double>.Build.Dense(n_dim, n_dim, F_01);
        for (int i = 0; i < n_dim; i++)
        {
            for (int j = 0; j < n_dim; j++)
            {
                A_mat[i, j] = 0.1 * (i + 1) * (j + 1) * (1.0 / (1.0 + Math.Exp(-x[j]))) + g * F[i, j];
            }
        }
        A_mat[0, 0] = 0.8;
        A_mat[1, 1] = 0.7;
        A_mat[2, 2] = 0.6;
        return A_mat;
    }

    static Vector<double> UpdateClassical(Vector<double> x, Vector<double> A_mu, Vector<double> b, double F_01, bool use_big)
    {
        var A_mu_eff = use_big ? A_mu + b : A_mu;
        var A_mat = ComputeA(x, A_mu_eff, F_01);
        var x_new = A_mat * x;
        if (use_big) x_new += b;
        for (int i = 0; i < n_dim; i++)
            x_new[i] += sigma * GaussianRandom();
        return x_new;
    }

    static Vector<MathNet.Numerics.Complex> UpdateQuantum(Vector<MathNet.Numerics.Complex> psi, Matrix<MathNet.Numerics.Complex> H0, Matrix<MathNet.Numerics.Complex> H_BIG, bool use_big)
    {
        var H = use_big ? H0 + H_BIG : H0;
        var U = Matrix<MathNet.Numerics.Complex>.Build.DenseIdentity(n_dim) - new MathNet.Numerics.Complex(0, 1) * H * (dt / hbar);
        var psi_new = U * psi;
        return psi_new / psi_new.Norm(2);
    }

    static double HappinessClassical(Vector<double> x)
    {
        return 1.0 / (1.0 + Math.Exp(-x.DotProduct(Vector<double>.Build.Dense(w))));
    }

    static MathNet.Numerics.Complex HappinessQuantum(Vector<MathNet.Numerics.Complex> psi, Matrix<MathNet.Numerics.Complex> H_op)
    {
        return psi.Conjugate().DotProduct(H_op * psi);
    }

    static MathNet.Numerics.Complex ComputeAExpect(Vector<MathNet.Numerics.Complex> psi, Matrix<MathNet.Numerics.Complex> A_op)
    {
        return psi.Conjugate().DotProduct(A_op * psi);
    }
}
