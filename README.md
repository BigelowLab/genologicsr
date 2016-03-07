## genologicsr

A package for navigating RESTful transactions and genealogy of the [Genologics](http://genologics.com/developer) Rapid Scripting API.  This R package is inspired by the Python module [genologics](https://github.com/SciLifeLab/genologics).  Thank you for sharing [SciLifeLab](http://www.scilifelab.se/)!

Like the python [genologics](https://github.com/SciLifeLab/genologics) module after which it is modeled, genologicsr provides reference classes to manage tansactions with LIMS and to work with the XML data representing database entities (files, sample, artifacts, processes, field, etc.).

Once of the nicer features of this is that the portion of the wrapper that handles the http transactions is isolated from working with the content (xml formed data).  So, future switches to other http transfer tools (away from httr and/or curl R packages) can be handled behind and opaque interface.  It's like changing your postal carrier without causing disrupting your mail service.

#### Wiki

See the [wiki](https://github.com/BigelowLab/genologicsr/wiki) for details and examples.


#### R package requirements

[XML](http://cran.r-project.org/web/packages/XML/index.html)

[httr](http://cran.r-project.org/web/packages/httr/index.html)

#### Installation

The configuration file can be saved in a user's home directory by any name, choosing a hidden file in the home directory makes a lot of sense.

Create a file, such as `~/.clarityrc` with the contents below and edit appropriately for your platform.

```
# edit as needed [genologics] section is required anyhting else is optional
[genologics]
BASEURI=http://my-server.my.host.org:8080
USERNAME=user
PASSWORD=password
VERSION=v2
[glsfilestore]
USERNAME=anotheruser
PASSWORD=anotherpassword
```

It's easy to install using Hadley Wickham's [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```R
library(devtools)
install_github("BigelowLab/genologicsr")

# or into the the system-wide library
with_lib(.Library, install_github("BigelowLab/genologicsr"))
```

#### Working with different API versions

Even though much of API v2 interoperates with API v1 there are some significant resources missing in v1 that are present in v2.  Some capabilities in `geneologicsr` adapt to the different versions without requiring any special effort on the part of the users. 
