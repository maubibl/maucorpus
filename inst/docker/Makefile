#! make

build:
	docker build -t kthcorpus -f Dockerfile ../..

test:
	# use any necessary envvars in .Renviron
	docker run --rm -it \
		--env-file ${HOME}/.Renviron \
			kthcorpus bash
