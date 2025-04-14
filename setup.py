from setuptools import setup, find_packages

setup(
    name="python_CM4",
    version="0.0.1",
    description="Python wrapper of CM4",
    author="Collin Kadlecek",
    author_email="collin.kadlecek@noaa.gov",
    packages=find_packages(),  # Automatically includes all Python packages inside CM4/
    include_package_data=True,
    install_requires=[
        "numpy==1.25.6",
        "geomaglib>=1.2.1",
        "datetime"
    ],
    classifiers=[
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.9",
)