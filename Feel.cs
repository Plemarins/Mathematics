using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Threading.Tasks;

namespace SensorimotorProcessing
{
    class Program
    {
        const int N_MODES = 3; // HMMモード数
        const int N_NODES = 5; // DBNノード数
        const int N_CONTEXT = 5; // コンテキスト履歴
        const int N_STATE = 4; // ダイナミックシステム状態
        const int N_HIDDEN = 20; // LSTM隠れユニット
        const int N_PARTICLES = 10; // DBN粒子数
        const double DT = 0.01; // 時間ステップ
        const double NOISE = 0.1; // ノイズ分散

        static Random rand = new Random();

        static void Main(string[] args)
        {
            var hmm = new HMM(N_MODES);
            var dbn = new DBN(N_PARTICLES, N_NODES);
            var context = new ContextLearning(N_CONTEXT);
            var dynamics = new DynamicSystem(N_STATE);
            var lstm = new LSTM(N_HIDDEN, 4, 2);

            double[] state = new double[N_STATE]; // theta1, theta2, dtheta1, dtheta2
            double[] sensorInput = new double[4]; // 目標x,y + 障害物x,y
            double[] motorOutput = new double[2]; // theta1, theta2
            double[] weights = new double[5] { 0.2, 0.2, 0.2, 0.2, 0.2 }; // フレームワーク重み
            double totalError = 0.0;
            int currentMode = 0;

            using (var writer = new StreamWriter("trajectory.csv"))
            {
                writer.WriteLine("Time,Theta1,Theta2,TargetX,TargetY,ObstacleX,ObstacleY,Error");

                for (int t = 0; t < 200; t++)
                {
                    // 目標と障害物の更新
                    double[] target = { Math.Cos(t * 0.05), Math.Sin(t * 0.05) };
                    double[] obstacle = { 0.5 * Math.Cos(t * 0.1), 0.5 * Math.Sin(t * 0.1) };
                    sensorInput[0] = target[0] + GaussianNoise(NOISE);
                    sensorInput[1] = target[1] + GaussianNoise(NOISE);
                    sensorInput[2] = obstacle[0];
                    sensorInput[3] = obstacle[1];

                    // 1. HMM
                    currentMode = hmm.Update(currentMode, sensorInput, state.Take(2).ToArray());
                    weights[0] = hmm.Confidence(currentMode);

                    // 2. DBN
                    motorOutput = dbn.Infer(sensorInput, state.Take(2).ToArray());
                    weights[1] = dbn.Confidence();

                    // 3. コンテキスト学習
                    context.Update(sensorInput);
                    motorOutput = context.Learn(motorOutput);
                    weights[2] = context.Confidence();

                    // 4. ダイナミックシステム
                    dynamics.Update(state, motorOutput, obstacle, DT);
                    weights[3] = 0.2; // 固定

                    // 5. LSTM
                    double[] lstmOutput = lstm.Forward(sensorInput);
                    weights[4] = 0.2; // 固定

                    // 動的加重統合
                    for (int i = 0; i < 2; i++)
                        motorOutput[i] = weights[1] * motorOutput[i] + weights[4] * lstmOutput[i];
                    motorOutput = motorOutput.Select(x => x / weights.Skip(1).Take(4).Sum()).ToArray();

                    // 誤差計算
                    double[] endEffector = dynamics.ForwardKinematics(state.Take(2).ToArray());
                    double error = Math.Sqrt(endEffector.Zip(target, (a, b) => (a - b) * (a - b)).Sum());
                    totalError += error;

                    // 出力
                    writer.WriteLine($"{t},{state[0]},{state[1]},{target[0]},{target[1]},{obstacle[0]},{obstacle[1]},{error}");
                    Console.WriteLine($"Time: {t}, Mode: {currentMode}, Error: {error:F4}");
                }
            }

            Console.WriteLine($"Average Error: {totalError / 200:F4}");
        }

        static double GaussianNoise(double sigma)
        {
            double u1 = rand.NextDouble();
            double u2 = rand.NextDouble();
            return sigma * Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Cos(2.0 * Math.PI * u2);
        }
    }

    class HMM
    {
        private double[,] trans;
        private double[,] mean;
        private double[] var;
        private int nModes;

        public HMM(int nModes)
        {
            this.nModes = nModes;
            trans = new double[nModes, nModes] { { 0.7, 0.2, 0.1 }, { 0.3, 0.5, 0.2 }, { 0.1, 0.3, 0.6 } };
            mean = new double[nModes, 2] { { 0.0, 0.0 }, { 0.5, 0.5 }, { -0.5, -0.5 } };
            var = new double[nModes] { 0.1, 0.2, 0.3 };
        }

        public int Update(int mode, double[] input, double[] state)
        {
            double[] prob = new double[nModes];
            for (int i = 0; i < nModes; i++)
            {
                double obsProb = Math.Exp(-((input[0] - state[0] - mean[i, 0]) * (input[0] - state[0] - mean[i, 0]) +
                                            (input[1] - state[1] - mean[i, 1]) * (input[1] - state[1] - mean[i, 1])) / (2 * var[i]));
                prob[i] = trans[mode, i] * obsProb;
            }
            prob = prob.Select(x => x / prob.Sum()).ToArray();
            double randVal = Program.rand.NextDouble(), cumProb = 0.0;
            for (int i = 0; i < nModes; i++)
            {
                cumProb += prob[i];
                if (randVal < cumProb) return i;
            }
            return nModes - 1;
        }

        public double Confidence(int mode)
        {
            return trans[mode, Enumerable.Range(0, nModes).MaxBy(i => trans[mode, i])];
        }
    }

    class DBN
    {
        private double[,] particles;
        private double[] weights;
        private int nParticles, nNodes;

        public DBN(int nParticles, int nNodes)
        {
            this.nParticles = nParticles;
            this.nNodes = nNodes;
            particles = new double[nParticles, nNodes];
            weights = Enumerable.Repeat(1.0 / nParticles, nParticles).ToArray();
        }

        public double[] Infer(double[] input, double[] state)
        {
            double[,] newParticles = new double[nParticles, nNodes];
            for (int i = 0; i < nParticles; i++)
            {
                newParticles[i, 0] = input[0] + Program.GaussianNoise(0.05);
                newParticles[i, 1] = input[1] + Program.GaussianNoise(0.05);
                newParticles[i, 2] = state[0];
                newParticles[i, 3] = state[1];
                newParticles[i, 4] = Math.Sqrt(Math.Pow(input[2] - ForwardKinematics(state)[0], 2) +
                                               Math.Pow(input[3] - ForwardKinematics(state)[1], 2));
                weights[i] = Math.Exp(-(Math.Pow(newParticles[i, 0] - input[0], 2) +
                                        Math.Pow(newParticles[i, 1] - input[1], 2)) / 0.1);
            }
            weights = weights.Select(x => x / weights.Sum()).ToArray();
            particles = newParticles;
            return Enumerable.Range(0, nParticles)
                .Select(i => new[] { particles[i, 2], particles[i, 3] }.Select(x => x.Select((v, j) => v * weights[i]).ToArray()))
                .Aggregate((a, b) => a.Zip(b, (x, y) => x + y).ToArray());
        }

        public double Confidence()
        {
            return weights.Sum() / nParticles;
        }

        private double[] ForwardKinematics(double[] theta)
        {
            double L1 = 1.0, L2 = 1.0;
            return new[]
            {
                L1 * Math.Cos(theta[0]) + L2 * Math.Cos(theta[0] + theta[1]),
                L1 * Math.Sin(theta[0]) + L2 * Math.Sin(theta[0] + theta[1])
            };
        }
    }

    class ContextLearning
    {
        private double[,] context;
        private int nContext;

        public ContextLearning(int nContext)
        {
            this.nContext = nContext;
            context = new double[4, nContext];
        }

        public void Update(double[] input)
        {
            for (int i = 0; i < nContext - 1; i++)
                for (int j = 0; j < 4; j++)
                    context[j, i] = context[j, i + 1];
            for (int j = 0; j < 4; j++)
                context[j, nContext - 1] = input[j];
        }

        public double[] Learn(double[] baseOutput)
        {
            double[] scores = new double[nContext];
            for (int i = 0; i < nContext; i++)
                scores[i] = context.Cast<double>().Skip(i * 4).Take(4).Zip(context.Cast<double>().Skip((nContext - 1) * 4).Take(4), (a, b) => a * b).Sum();
            double[] attention = scores.Select(x => Math.Exp(x) / scores.Sum(y => Math.Exp(y))).ToArray();
            double[] output = baseOutput.ToArray();
            for (int i = 0; i < nContext; i++)
                for (int j = 0; j < 2; j++)
                    output[j] += attention[i] * context[j, i];
            return output.Select(x => x / (attention.Sum() + 1.0)).ToArray();
        }

        public double Confidence()
        {
            return 0.2; // 固定
        }
    }

    class DynamicSystem
    {
        private int nState;

        public DynamicSystem(int nState)
        {
            this.nState = nState;
        }

        public void Update(double[] state, double[] input, double[] obstacle, double dt)
        {
            double[] endEffector = ForwardKinematics(state.Take(2).ToArray());
            double dist = Math.Sqrt(obstacle.Zip(endEffector, (a, b) => (a - b) * (a - b)).Sum());
            double[] avoidance = dist < 0.3 ? obstacle.Zip(endEffector, (a, b) => 0.1 * (b - a) / dist).ToArray() : new double[2];
            double Kp = 2.0, Kd = 0.5;
            double[] torque = new double[2];
            for (int i = 0; i < 2; i++)
                torque[i] = Kp * (input[i] - state[i]) - Kd * state[i + 2] + avoidance[i];
            for (int i = 0; i < 2; i++)
            {
                state[i + 2] += dt * torque[i];
                state[i] += dt * state[i + 2];
            }
        }

        public double[] ForwardKinematics(double[] theta)
        {
            double L1 = 1.0, L2 = 1.0;
            return new[]
            {
                L1 * Math.Cos(theta[0]) + L2 * Math.Cos(theta[0] + theta[1]),
                L1 * Math.Sin(theta[0]) + L2 * Math.Sin(theta[0] + theta[1])
            };
        }
    }

    class LSTM
    {
        private double[] hidden, cell;
        private double[,] Wih, Whh, Who;
        private int nHidden, nInput, nOutput;

        public LSTM(int nHidden, int nInput, int nOutput)
        {
            this.nHidden = nHidden;
            this.nInput = nInput;
            this.nOutput = nOutput;
            hidden = new double[nHidden];
            cell = new double[nHidden];
            Wih = new double[nHidden, nInput];
            Whh = new double[nHidden, nHidden];
            Who = new double[nOutput, nHidden];
            Random rand = new Random();
            for (int i = 0; i < nHidden; i++)
            {
                for (int j = 0; j < nInput; j++) Wih[i, j] = rand.NextDouble() * 0.1;
                for (int j = 0; j < nHidden; j++) Whh[i, j] = rand.NextDouble() * 0.1;
                for (int j = 0; j < nOutput; j++) Who[j, i] = rand.NextDouble() * 0.1;
            }
        }

        public double[] Forward(double[] input)
        {
            double[] forget = new double[nHidden], inputGate = new double[nHidden],
                     outputGate = new double[nHidden], candidate = new double[nHidden];
            for (int i = 0; i < nHidden; i++)
            {
                for (int j = 0; j < nInput; j++)
                {
                    forget[i] += Wih[i, j] * input[j];
                    inputGate[i] += Wih[i, j] * input[j];
                    outputGate[i] += Wih[i, j] * input[j];
                    candidate[i] += Wih[i, j] * input[j];
                }
                for (int j = 0; j < nHidden; j++)
                {
                    forget[i] += Whh[i, j] * hidden[j];
                    inputGate[i] += Whh[i, j] * hidden[j];
                    outputGate[i] += Whh[i, j] * hidden[j];
                    candidate[i] += Whh[i, j] * hidden[j];
                }
                forget[i] = 1.0 / (1.0 + Math.Exp(-forget[i]));
                inputGate[i] = 1.0 / (1.0 + Math.Exp(-inputGate[i]));
                outputGate[i] = 1.0 / (1.0 + Math.Exp(-outputGate[i]));
                candidate[i] = Math.Tanh(candidate[i]);
            }
            for (int i = 0; i < nHidden; i++)
            {
                cell[i] = forget[i] * cell[i] + inputGate[i] * candidate[i];
                hidden[i] = outputGate[i] * Math.Tanh(cell[i]);
            }
            double[] output = new double[nOutput];
            for (int i = 0; i < nOutput; i++)
                for (int j = 0; j < nHidden; j++)
                    output[i] += Who[i, j] * hidden[j];
            return output;
        }
    }
}
