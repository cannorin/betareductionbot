MONO_PATH?=/usr/bin

EX_NUGET:=nuget/bin/nuget

XBUILD?=$(MONO_PATH)/xbuild
MONO?=$(MONO_PATH)/mono
GIT?=$(shell which git)

NUGET?=$(EX_NUGET)

all: binary ;

binary: nuget-packages-restore 
	$(XBUILD) BetaReductionBot.sln /p:Configuration=Release

# External tools

external-tools: nuget ;

nuget: $(NUGET) ;

submodule:
	$(GIT) submodule update --init --recursive

$(EX_NUGET): submodule
	cd nuget && $(MAKE)

# NuGet

nuget-packages-restore: external-tools
	[ -d packages ] || \
	    $(NUGET) restore -ConfigFile BetaReductionBot/packages.config -PackagesDirectory packages ; \

# Clean

clean:
	$(RM) -rf BetaReductionBot/obj

