using Random
using Plots
using DataFrames
using Printf

# アイテム構造体
mutable struct ScalingItem
    name::String
    is_useful::Bool
    critical_usage::Int
    current_usage::Int
    usefulness_score::Float64
    fluctuation::Float64
end

# シミュレーション関数
function simulate_scaling_law()
    # 初期化
    item = ScalingItem("SoftwarePatch", true, 5, 0, 1.0, 0.0)
    beta = 0.5    # スケーリング指数 β
    gamma = 0.8   # 揺らぎのスケーリング指数 γ
    max_usage = 9 # 最大使用回数
    delta = 2     # 臨界状態の範囲
    
    # データフレームで結果を保存
    df = DataFrame(
        UsageCount = Int[],
        ItemName = String[],
        Usefulness = String[],
        UsefulnessScore = Float64[],
        Fluctuation = Float64[]
    )
    
    # シミュレーション
    for usage in 0:max_usage
        if usage >= item.critical_usage
            # 臨界点以降：有用性ゼロ
            item.is_useful = false
            item.usefulness_score = 0.0
            item.fluctuation = 0.0
        else
            # 臨界点手前：スケーリング法則
            distance = item.critical_usage - usage
            # 有用性：U ~ (u_c - u)^(-β)
            item.usefulness_score = (distance ^ (-beta)) / (item.critical_usage ^ (-beta)) # 正規化
            item.usefulness_score = min(item.usefulness_score, 1.0)
            
            # 揺らぎ：σ ~ (u_c - u)^(-γ)
            item.fluctuation = 0.1 * (distance ^ (-gamma))
            if usage >= item.critical_usage - delta
                # 臨界点付近：ランダム揺らぎ
                item.usefulness_score += randn() * item.fluctuation
                item.usefulness_score = clamp(item.usefulness_score, 0.0, 1.0)
            end
        end
        
        # 結果をデータフレームに追加
        push!(df, (
            usage,
            item.name,
            item.is_useful ? "Useful" : "Not Useful",
            item.usefulness_score,
            item.fluctuation
        ))
        
        item.current_usage = usage
    end
    
    # 表形式で出力
    println("Simulation Results:")
    for row in eachrow(df)
        @printf("Usage: %2d, Item: %s, Usefulness: %s, Score: %.2f, Fluctuation: %.2f\n",
            row.UsageCount, row.ItemName, row.Usefulness, row.UsefulnessScore, row.Fluctuation)
    end
    
    # グラフの作成
    p = plot(
        df.UsageCount,
        df.UsefulnessScore,
        label="Usefulness Score",
        xlabel="Usage Count",
        ylabel="Usefulness Score",
        title="Scaling Law near Critical Point",
        linewidth=2,
        color=:blue
    )
    plot!(
        p,
        df.UsageCount,
        df.Fluctuation,
        label="Fluctuation",
        ylabel="Fluctuation",
        yaxis=:right,
        linewidth=2,
        color=:red
    )
    
    # グラフを表示
    display(p)
    
    return df
end

# シミュレーション実行
simulate_scaling_law()
