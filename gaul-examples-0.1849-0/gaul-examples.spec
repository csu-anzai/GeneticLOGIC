Name:		gaul-examples
Summary:	Example programs for GAUL
Version:	0.1849
Release:	0
License:	Copyright (c) 2005, Stewart Adcock.  Licensed under terms of the GNU GPL.
Vendor:		Stewart Adcock
Packager:	"Stewart Adcock" <gaul@linux-domain.com>
Group:		Scientific/Engineering
Source:		gaul-examples-%{PACKAGE_VERSION}-%{PACKAGE_RELEASE}.tar.gz
URL:		http://gaul.sourceforge.net/
BuildRoot:	%{_tmppath}/gaul-examples-%{PACKAGE_VERSION}-%{PACKAGE_RELEASE}-buildroot

########################################################################
# gaul-examples/gaul-examples.spec
########################################################################
#
# GAUL - Genetic Algorithm Utility Library
# Copyright ©2005, Stewart Adcock <stewart@linux-domain.com>
# All rights reserved.
#
# The latest version of this program should be available at:
# http://gaul.sourceforge.net/
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.  Alternatively, if your project
# is incompatible with the GPL, I will probably agree to requests
# for permission to use the terms of any other license.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY WHATSOEVER.
#
# A full copy of the GNU General Public License should be in the file
# "COPYING" provided with this distribution; if not, see:
# http://www.gnu.org/
#
########################################################################

%description
The Genetic Algorithm Utility Library (GAUL) is a flexible open source programming library providing evolutionary algorithms.  Steady-state, generational and island model genetic algorithms are supported, using Darwinian, Lamarckian or Baldwinian evolution.  Other evolutionary algorithms, such as differential evolution and deterministic crowding, are fully supported.  Standard mutation, crossover and selection operators are provided, while code hooks additionally allow custom operators.  Several non-evolutionary search heuristics are also provided for comparison and local search purposes, including simplex method, hill climbing, simulated annealling and steepest ascent.  Includes support for multiprocessor and distributed systems.  Much of the functionality is accessible through a simple S-Lang interface.

%prep
%setup -n gaul-examples-%{PACKAGE_VERSION}-%{PACKAGE_RELEASE}

%build
./configure --prefix=%{_prefix} --libdir=%{_libdir}
make

%install
rm -rf ${RPM_BUILD_ROOT}
%makeinstall

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-, root, root)
%{_bindir}/*
%doc AUTHORS COPYING ChangeLog NEWS README

%changelog
* Fri Apr 22 2005 Stewart Adcock <stewart@linux-domain.com>
- 0.1849-0
- Fixed by removing extraneous lib dir and include dir in files section.

* Mon Apr 11 2005 Stewart Adcock <stewart@linux-domain.com>
- 0.1848-1
- Examples split from the main gaul-devel package.
