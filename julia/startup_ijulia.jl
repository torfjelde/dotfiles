if VERSION ≥ v"1.7"
    try
        @eval using Revise
    catch e
        @warn "Error initializing Revise" exception=(e, catch_backtrace())
    end
end

# HACK: Activate test environment if we're in a test environment.
# Using an environment variable to indicate this.
if haskey(ENV, "TOR_JULIA_TEST") && ENV["TOR_JULIA_TEST"] == "true"
    if basename(dirname(Base.active_project())) == "test"
        using Pkg
        Pkg.activate("..")
    end
    using TestEnv; TestEnv.activate()
end
