import setuptools

setuptools.setup(
    name="ip2region",
    version="3.0.0",
    description="official python binding for ip2region with both IPv4 and IPv6 supported",
    long_description=open("ReadMe.md", encoding='utf-8').read(),
    long_description_content_type='text/markdown',

    url="https://github.com/lionsoul2014/ip2region",
    license="Apache-2.0 License",
    author_email="chenxin619315@gmail.com",
    author="lionsoul2014",

    packages=setuptools.find_packages(),
    include_package_data=True,
    install_requires=[],
    classifiers=(
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: Apache Software License",
        "Operating System :: OS Independent",
    ),
)
