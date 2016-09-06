#!/usr/bin/env python
import argparse
import os
import shutil
import subprocess

def make_path(*paths):
    return os.path.abspath(os.path.join(*paths))

def check_call(command, cwd=None):
    subprocess.check_call(command, cwd=cwd)

def check_call_read(command, cwd=None):
    p = subprocess.Popen(command, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    output, error = p.communicate()
    if p.returncode != 0:
        raise RuntimeError("Command failed with status {} (command={})".format(p.returncode, command))
    return output.strip()

def do_clean(args, project_dir):
    check_call(["stack", "clean"], cwd=project_dir)

def do_build(args, project_dir):
    config_upper = args.config.upper()
    check_call(["stack", "build", "--ghc-options=\"-D{}\"".format(config_upper)], cwd=project_dir)

def do_generate(args, project_dir):
    check_call(["stack", "exec", "site", "rebuild"], cwd=project_dir)

def do_check(args, project_dir):
    check_call(["diff", "-r", "_site_{}".format(args.config), "_site"], cwd=project_dir)

def do_deploy(args, project_dir):
    def delete_contents(dir):
        for root, dirs, file_names in os.walk(dir):
            for file_name in file_names:
                os.unlink(os.path.join(root, file_name))
            for d in dirs:
                shutil.rmtree(os.path.join(root, d))

    def copy_contents(src, dst):
        for item in os.listdir(src):
            s = os.path.join(src, item)
            d = os.path.join(dst, item)
            if os.path.isdir(s):
                shutil.copytree(s, d)
            else:
                shutil.copy(s, d)

    site_dir = make_path(project_dir, "_site")
    if os.path.isdir(args.output_dir):
        delete_contents(args.output_dir)
        copy_contents(site_dir, args.output_dir)
    else:
        shutil.copytree(site_dir, args.output_dir)

if __name__ == "__main__":
    def add_config_arg(parser):
        parser.add_argument("-c", "--config", default="release", choices=["debug", "release"], help="configuration")

    project_dir = check_call_read(["git", "rev-parse", "--show-toplevel"])

    parser = argparse.ArgumentParser(description="Build and deploy content")
    subparsers = parser.add_subparsers(help="subcommand help")

    clean_parser = subparsers.add_parser("clean", help="clean")
    clean_parser.set_defaults(func=do_clean)

    build_parser = subparsers.add_parser("build", help="build")
    build_parser.set_defaults(func=do_build)
    add_config_arg(build_parser)

    generate_parser = subparsers.add_parser("generate", help="generate")
    generate_parser.set_defaults(func=do_generate)

    check_parser = subparsers.add_parser("check", help="check")
    check_parser.set_defaults(func=do_check)
    add_config_arg(check_parser)

    deploy_parser = subparsers.add_parser("deploy", help="deploy")
    deploy_parser.set_defaults(func=do_deploy)
    deploy_parser.add_argument("output_dir", metavar="OUTPUTDIR", help="output directory")

    args = parser.parse_args()
    args.func(args, project_dir)
