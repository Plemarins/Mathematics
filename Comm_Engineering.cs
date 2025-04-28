using System;
using System.IO;

class CommEngineering
{
    // 定数
    const double G0 = 6.67430E-11;        // 万有引力定数 (m^3 kg^-1 s^-2)
    const double C0 = 2.99792458E8;       // 光速 (m/s)
    const double HBAR0 = 1.0545718E-34;   // プランク定数 (J s)
    const double KB = 1.380649E-23;       // ボルトン定数 (J/K)
    const double M_PION = 139.570E6 * 1.60217662E-19 / (C0 * C0); // パイオン質量 (kg)
    const double M_GRAVITON = 1.0E-30;    // グラビトン質量 (kg)
    const double EPSILON = 1.0E-10;       // ゼロ除算回避
    const int N_STEPS = 100;              // 距離ステップ数

    static void Main()
    {
        // 初期化
        double alpha_G = 1.0, alpha_c = 1.0, alpha_hbar = 1.0;
        double G = alpha_G * G0;
        double c = alpha_c * C0;
        double hbar = alpha_hbar * HBAR0;
        double v_rel = 0.1 * c;
        double beta = v_rel / c;
        double gamma = 1.0 / Math.Sqrt(1.0 - beta * beta);
        double l_planck = Math.Sqrt(hbar * G / (c * c * c));
        double t_planck = Math.Sqrt(hbar * G / (c * c * c * c * c));
        double m_planck = Math.Sqrt(hbar * c / G);
        double dr = 1.0E-15;

        // ファイル出力
        using (StreamWriter writer = new StreamWriter("comm_engineering.csv"))
        {
            writer.WriteLine("Distance(m),V_grav(J),V_yukawa(J),V_gauge(J),V_total(J),Entropy(bits),Info_Capacity(bits/s),Honesty,Justice,Prudence");

            // コンソールヘッダ
            Console.WriteLine("Simulation Results:");
            Console.WriteLine("Distance(m),V_grav(J),V_yukawa(J),V_gauge(J),V_total(J),Entropy(bits),Info_Capacity(bits/s),Honesty,Justice,Prudence");

            // 距離ループ
            for (int i = 1; i <= N_STEPS; i++)
            {
                double r = i * dr;
                double r_prime = gamma * (r - v_rel * t_planck);
                if (r_prime < EPSILON) r_prime = EPSILON;

                // 万有引力（斥力拡張）
                double V_grav = -G * 1.0 * 1.0 * Math.Exp(-M_GRAVITON * c * r_prime / hbar) / r_prime;
                if (1.0 * 1.0 < 0) V_grav = -V_grav; // 反質量で斥力

                // 湯川相互作用（引力的）
                double V_yukawa = -1.0 * Math.Exp(-M_PION * c * r_prime / hbar) / r_prime;

                // ゲージ相互作用（スピン1：同電荷で斥力、異電荷で引力）
                double V_gauge;
                if (1.0 * 1.0 > 0)
                    V_gauge = 1.0 * Math.Exp(-1.0E-18 * c * r_prime / hbar) / r_prime; // 斥力
                else
                    V_gauge = -1.0 * Math.Exp(-1.0E-18 * c * r_prime / hbar) / r_prime; // 引力

                // 総ポテンシャル
                double V_total = V_grav + V_yukawa + V_gauge;

                // 情報熱力学：エントロピー計算
                double prob_attr = Math.Exp(-Math.Abs(V_total) / (1.0E-20 + EPSILON));
                double prob_rep = 1.0 - prob_attr;
                double entropy = 0.0;
                if (prob_attr > 0 && prob_rep > 0)
                {
                    entropy = -prob_attr * Math.Log(prob_attr, 2) - prob_rep * Math.Log(prob_rep, 2);
                }

                // 情報容量
                double info_capacity = Math.Min(c / (r_prime + EPSILON) * (1.0 - entropy), 1.0E12);

                // 徳倫理学の評価
                double honesty = 1.0 / (1.0 + Math.Abs(V_total - (V_grav + V_yukawa + V_gauge)));
                double justice = 1.0 / (1.0 + Math.Abs(1.0 - 1.0));
                double prudence = 1.0 / (1.0 + entropy);

                // ファイル出力
                writer.WriteLine($"{r_prime:E6},{V_grav:E6},{V_yukawa:E6},{V_gauge:E6},{V_total:E6},{entropy:E6},{info_capacity:E6},{honesty:E6},{justice:E6},{prudence:E6}");

                // コンソール出力
                Console.WriteLine($"{r_prime:E6},{V_grav:E6},{V_yukawa:E6},{V_gauge:E6},{V_total:E6},{entropy:E6},{info_capacity:E6},{honesty:E6},{justice:E6},{prudence:E6}");
            }
        }

        Console.WriteLine("Simulation completed. Results written to comm_engineering.csv");
    }
}
