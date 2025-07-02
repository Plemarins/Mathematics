using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Linq;
using MathNet.Numerics.LinearAlgebra;
using MathNet.Numerics.Optimization;
using Newtonsoft.Json;
using Microsoft.AspNetCore.Builder;
using Microsoft.Extensions.Hosting;

var builder = WebApplication.CreateBuilder(args);
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();
var app = builder.Build();

app.UseSwagger();
app.UseSwaggerUI();

public class NumerologyRequest
{
    public int[] Numbers { get; set; }
    public int[] Counts { get; set; }
}

public class NumerologyResponse
{
    public double[] OptimizedWeights { get; set; }
    public double[] Predictions { get; set; }
    public string PlotUrl { get; set; }
}

// 数秘術モデル
public class NumerologyModel
{
    private const int MaxK = 100; // 最大数値
    private const double Lambda = 0.1; // L1正則化係数
    private const double LearningRate = 0.01;
    private const int MaxIterations = 1000;

    public static (double[], double[]) Compute(int[] numbers, int[] counts)
    {
        // データ検証
        if (numbers.Length != counts.Length || numbers.Length == 0)
            throw new ArgumentException("Invalid input data");

        // 1. 最尤推定（ポアソン分布）
        var w_mle = new double[MaxK];
        for (int i = 0; i < numbers.Length; i++)
        {
            int k = numbers[i] - 1; // 1-based to 0-based
            if (k >= 0 && k < MaxK)
                w_mle[k] += Math.Log(Math.Max(1.0, counts[i]));
        }
        double sum_mle = w_mle.Sum();
        if (sum_mle > 0)
            for (int k = 0; k < MaxK; k++)
                w_mle[k] /= sum_mle;

        // 2. 最適化（勾配降下法）
        var w_opt = (double[])w_mle.Clone();
        for (int iter = 0; iter < MaxIterations; iter++)
        {
            var grad = new double[MaxK];
            for (int k = 0; k < MaxK; k++)
            {
                grad[k] = 2.0 * (w_opt[k] - w_mle[k]) + Lambda * Math.Sign(w_opt[k]);
            }

            // 更新
            for (int k = 0; k < MaxK; k++)
                w_opt[k] -= LearningRate * grad[k];

            // 正規化
            double sum_w = w_opt.Sum();
            if (Math.Abs(sum_w) > 1e-6)
                for (int k = 0; k < MaxK; k++)
                    w_opt[k] /= sum_w;

            // 収束判定
            if (grad.Select(g => Math.Abs(g)).Sum() / MaxK < 1e-6)
                break;
        }

        // 3. 簡易予測（線形回帰風）
        var predictions = new double[MaxK];
        for (int k = 0; k < MaxK; k++)
        {
            predictions[k] = w_opt[k] * 1.1; // 簡易スケーリング
        }

        return (w_opt, predictions);
    }
}

// APIエンドポイント
app.MapPost("/api/numerology", ([FromBody] NumerologyRequest request) =>
{
    try
    {
        var (w_opt, predictions) = NumerologyModel.Compute(request.Numbers, request.Counts);

        // プロット生成（オプション、ScottPlotを使用する場合）
        // ここでは簡略化のため、仮のURLを返す
        string plotUrl = "http://localhost:5000/numerology_plot.png";

        return Results.Ok(new NumerologyResponse
        {
            OptimizedWeights = w_opt,
            Predictions = predictions,
            PlotUrl = plotUrl
        });
    }
    catch (Exception ex)
    {
        return Results.BadRequest(new { Error = ex.Message });
    }
});

app.Run();
