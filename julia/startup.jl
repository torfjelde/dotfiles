function template(interactive=false)
    @eval begin
        using PkgTemplates
        plugins = [
            ProjectFile(),
            SrcDir(),  # template file for the `src/<module>.jl` file
            Tests(project=true),  # create a separate project for tests
            Readme(),
            License(),
            Git(ssh=true, jl=true, manifest=false, ignore=["test/Manifest.toml"]),
            CompatHelper(cron="0 0 * * *"),  # run once every day
            TagBot(),
            GitHubActions(extra_versions=["1.6", "1"]),  # `1` should correspond to the latest version
            Documenter{GitHubActions}(),  # TODO: Add some default themeing?
            Citation(),
            RegisterAction(),
        ]
        Template(; plugins=plugins, interactive=$(interactive))
    end
end
