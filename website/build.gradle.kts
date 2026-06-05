import com.pswidersk.gradle.python.uv.UvxTask

plugins {
    base
    // https://github.com/PrzemyslawSwiderski/python-uv-gradle-plugin
    id("com.pswidersk.python-uv-plugin") version "0.4.32"
}

tasks.register<UvxTask>("website") {
    args = listOf(
        "--from", "sphinx",
        "--with", "sphinxcontrib-mermaid",
        "sphinx-build",
        "-M", "html",
        "src/site/sphinx", "build"
    )
}

tasks.assemble {
    dependsOn("website")
}
