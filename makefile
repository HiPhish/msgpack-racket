# Copyright 2017-2018 Alejandro Sanchez
#
# This file is part of MessagePack.rkt
# 
#     MessagePack.rkt is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by the
#     Free Software Foundation, either version 3 of the License, or (at your
#     option) any later version.
# 
#     MessagePack.rkt is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
#     Public License for more details.
# 
#     You should have received a copy of the GNU General Public License along
#     with MessagePack.rkt.  If not, see <http://www.gnu.org/licenses/>.

# ===[ Public variables ]=====================================================
RACO = raco


# ===[ Private variables ]====================================================
name = msgpack


# ===[ Phony targets ]========================================================
.PHONY: help install

help:
	@echo 'Usage: make (help | install | uninstall)'
	@echo '  help     Print this message'
	@echo '  install  Install the MessagePack library as a Racket directory package'
	@echo '  remove   Uninstall the MessagePack library Racket package'
	@echo 'The following variables can be specified:'
	@echo '  $$(RACO)  Binary for raco, defaults to "raco"'


install: msgpack
	@$(RACO) pkg install --type dir --name $(name) $<

remove:
	@$(RACO) pkg remove $(name)
