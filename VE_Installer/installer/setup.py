# setup.py
from distutils.core import setup
import py2exe

setup(windows=[{"script": "velauncher.py",
                "icon_resources": [(1, "installerImages/Ve_icon.ico")]}],
      zipfile=None)
