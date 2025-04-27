using Distributed, DifferentialEquations, LinearAlgebra, Random, Plots, QuantumOptics

# 並列処理の初期化
addprocs(4)
@everywhere using DifferentialEquations, QuantumOptics

# パラメータ設定
@everywhere begin
    const T, dt, σ, Δ, w, λ, ħ, g = 100.0, 0.1, 0.1, 0.5, [0.4, 0.3, 0.2], 0.5, 1.0, 0.1
end

# 古典的遷移行列（ゲージ場依存）
@everywhere function A(x, A_mu)
    a = zeros(3, 3)
    F = [0.0 0.0 0.0; 0.0 0.0 0.0; 0.0 0.0 0.0]  # 簡略化した場強度
    for i in 1:3, j in 1:3
        a[i,j] = 0.1 * i * j * (1 / (1 + exp(-x[j]))) + g * F[i,j]
    end
    a[1,1], a[2,2], a[3,3] = 0.8, 0.7, 0.6
    return a
end

# 幸福感計算
@everywhere function happiness_classical(x) return 1 / (1 + exp(-dot(w, x))) end
@everywhere function happiness_quantum(ψ, H_op) return real(expect(H_op, ψ)) end

# ダイナミクス
@everywhere function dynamics!(dx, x, p, t)
    A_mu = p[1]
    dx .= A(x, A_mu) * x + randn(3) * σ
end

@everywhere function dynamics_big!(dx, x, p, t)
    A_mu = p[1] + [Δ, 0, 0]
    b = [Δ, 0, 0]
    dx .= A(x, A_mu) * x + b + randn(3) * σ
end

@everywhere function quantum_dynamics(tspan, ψ0, H0, H_BIG, BIG::Bool)
    b = Basis(3)
    H = BIG ? H0 + H_BIG : H0
    tout, ψt = timeevolution.schroedinger(tspan, ψ0, H)
    return tout, ψt
end

# シミュレーション
function run_simulation()
    x0 = [0.0, 0.0, 0.0]
    tspan = (0.0, T)
    A_mu0 = [0.0, 0.0, 0.0]

    prob = ODEProblem(dynamics!, x0, tspan, [A_mu0])
    sol = solve(prob, Tsit5(), dt=dt, saveat=dt)
    prob_big = ODEProblem(dynamics_big!, x0, tspan, [A_mu0])
    sol_big = solve(prob_big, Tsit5(), dt=dt, saveat=dt)

    b = Basis(3)
    ψ0 = (1/sqrt(3)) * (Ket(b, [1,0,0]) + Ket(b, [0,1,0]) + Ket(b, [0,0,1]))
    H0 = w[1]*projector(Ket(b, [1,0,0])) + w[2]*projector(Ket(b, [0,1,0])) + w[3]*projector(Ket(b, [0,0,1]))
    H_BIG = Δ * projector(Ket(b, [1,0,0]))

    tout, ψt = quantum_dynamics(tspan, ψ0, H0, H_BIG, false)
    tout_big, ψt_big = quantum_dynamics(tspan, ψ0, H0, H_BIG, true)

    H_classical = [happiness_classical(sol[i]) for i in 1:length(sol)]
    H_classical_big = [happiness_classical(sol_big[i]) for i in 1:length(sol_big)]
    H_quantum = [happiness_quantum(ψt[i], H0) for i in 1:length(ψt)]
    H_quantum_big = [happiness_quantum(ψt_big[i], H0) for i in 1:length(ψt_big)]
    H_total = λ * H_classical + (1-λ) * H_quantum
    H_total_big = λ * H_classical_big + (1-λ) * H_quantum_big

    return sol, sol_big, ψt, ψt_big, H_total, H_total_big
end

# 並列実行と可視化
@time results = pmap(i -> run_simulation(), 1:4)
sol, sol_big, ψt, ψt_big, H_total, H_total_big = results[1]
t = 0:dt:T
plot(t, H_total, label="Happiness (No BIG)", xlabel="Time", ylabel="Happiness")
plot!(t, H_total_big, label="Happiness (With BIG)")
savefig("happiness_gauge_simulation.png")
