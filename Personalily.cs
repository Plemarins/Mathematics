using System;

namespace Personality
{
    class Program
    {
        // 定数定義
        private const int MBTI_DIM = 4;
        private const int DISC_DIM = 4;
        private const int MBTI_TYPES = 16;

        // MBTIとDISCを管理するクラス
        class PersonalityModel
        {
            private int[,] mbti; // MBTIタイプ（4次元バイナリベクトル）
            private double[,] disc; // DISCプロファイル（4次元実数ベクトル）

            public PersonalityModel()
            {
                // MBTIタイプの初期化（16タイプ）
                mbti = new int[MBTI_TYPES, MBTI_DIM]
                {
                    {0,0,0,0}, {1,0,0,0}, {0,1,0,0}, {1,1,0,0}, // ENFJ, INFJ, ENFP, INFP
                    {0,0,1,0}, {1,0,1,0}, {0,1,1,0}, {1,1,1,0}, // ENTJ, INTJ, ENTP, INTP
                    {0,0,0,1}, {1,0,0,1}, {0,1,0,1}, {1,1,0,1}, // ESFJ, ISFJ, ESFP, ISFP
                    {0,0,1,1}, {1,0,1,1}, {0,1,1,1}, {1,1,1,1}  // ESTJ, ISTJ, ESTP, ISTP
                };

                // DISCプロファイルの初期化（MBTIから簡略マッピング）
                disc = new double[MBTI_TYPES, DISC_DIM];
                for (int i = 0; i < MBTI_TYPES; i++)
                {
                    for (int j = 0; j < DISC_DIM; j++)
                    {
                        // MBTIのバイナリ値を0.25-0.75の範囲にスケーリング
                        disc[i, j] = mbti[i, j] * 0.5 + 0.25;
                    }
                }
            }

            // MBTIの群演算
            public void MbtiGroupOperation(int idx1, int idx2)
            {
                int[] result = new int[MBTI_DIM];
                for (int i = 0; i < MBTI_DIM; i++)
                {
                    result[i] = (mbti[idx1, i] + mbti[idx2, i]) % 2; // Z/2Zの加法
                }
                Console.WriteLine("MBTI Group Operation Result: [" + string.Join(", ", result) + "]");
            }

            // DISCのベクトル演算（合計）
            public double DiscVectorSum(int idx)
            {
                double sum = 0.0;
                for (int i = 0; i < DISC_DIM; i++)
                {
                    sum += disc[idx, i];
                }
                return sum;
            }

            // 関手 F: MBTI → DISC の表示
            public void DisplayDiscProfile(int idx)
            {
                double[] profile = new double[DISC_DIM];
                for (int i = 0; i < DISC_DIM; i++)
                {
                    profile[i] = disc[idx, i];
                }
                Console.WriteLine("DISC Profile: [" + string.Join(", ", profile) + "]");
            }

            // MBTIタイプの取得
            public int[] GetMbtiType(int idx)
            {
                int[] type = new int[MBTI_DIM];
                for (int i = 0; i < MBTI_DIM; i++)
                {
                    type[i] = mbti[idx, i];
                }
                return type;
            }
        }

        static void Main(string[] args)
        {
            /*
             * ZFC風公理:
             * 公理1: プロファイル集合 P = M ∪ D, M ≅ (Z/2Z)^4, D ⊆ R^4
             * 公理2: 任意の x, y ∈ P に対し、Hom(x,y) が存在
             * 公理3: P は圏 (P, Hom, ∘) を形成
             * 公理4: M は16元の群、M ≅ (Z/2Z)^4
             * 公理5: D は R^4 のベクトル空間
             * 公理6: M → D の関手 F が存在
             * 公理7: M は D に埋め込まれる
             */

            // モデル初期化
            PersonalityModel model = new PersonalityModel();

            // MBTIの群演算（例: ISTJ + ENFP）
            Console.WriteLine("MBTI Group Operation (Example: ISTJ + ENFP)");
            model.MbtiGroupOperation(15, 2); // ISTJ (index 15, 0-based), ENFP (index 2)

            // DISCのベクトル演算（例: ENFJのスコア合計）
            double discSum = model.DiscVectorSum(0); // ENFJ (index 0)
            Console.WriteLine($"DISC Vector Sum (ENFJ): {discSum}");

            // 関手の例: MBTI → DISC（ISTJのDISCプロファイル）
            Console.WriteLine("Functor F: MBTI(ISTJ) → DISC");
            model.DisplayDiscProfile(15); // ISTJ (index 15)
        }
    }
}
