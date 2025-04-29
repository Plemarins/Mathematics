using LinearAlgebra
using Random
using Distributed

# Parameters
const k = 2        # Dimension of mind state
const n = 3        # Number of phenomena archetypes
const α = 0.01     # Learning rate
const n_iter = 100 # Number of iterations

# Compute probability matrix
function prob_matrix(m::Vector{Float64}, e::Matrix{Float64})
    scores = e * m  # Dot product for each phenomenon
    exps = exp.(scores)
    probs = exps / sum(exps)
    return probs
end

# Sample phenomenon
function sample_phenomenon(probs::Vector{Float64}, e::Matrix{Float64}, rng)
    r = rand(rng)
    cum_probs = cumsum(probs)
    idx = findfirst(x -> x >= r, cum_probs)
    return e[idx, :]
end

# Update mind state
function update_mind(m::Vector{Float64}, e_t::Vector{Float64})
    return m + α * e_t
end

# One step
function step(m::Vector{Float64}, e::Matrix{Float64}, rng)
    probs = prob_matrix(m, e)
    e_t = sample_phenomenon(probs, e, rng)
    m_new = update_mind(m, e_t)
    return m_new
end

# Simulate
function simulate(m0::Vector{Float64}, e::Matrix{Float64}, seed::Int, n_iter::Int)
    rng = Random.MersenneTwister(seed)
    m = copy(m0)
    for _ in 1:n_iter
        m = step(m, e, rng)
    end
    return m
end

# Main
function main()
    # Initialize
    m0 = [0.5, 0.5]  # Initial mind state
    e = [1.0 0.0;     # Phenomena archetypes
         0.0 1.0;
         1.0 1.0]
    seed = 42

    # Run simulation
    m_final = simulate(m0, e, seed, n_iter)

    # Print results
    println("Initial mind state: ", m0)
    println("Final mind state: ", m_final)
end

# Run
main()
