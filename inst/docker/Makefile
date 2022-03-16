#! make

build:
	docker build -t kthcorpus -f Dockerfile ../..

test:
	# use any necessary envvars in .Renviron
	docker run --rm -it \
		--env-file ${HOME}/.Renviron \
			kthcorpus bash

test2:
	# use any necessary envvars in .Renviron
	docker run --rm -it \
		--env-file ${HOME}/.Renviron \
		-v "${HOME}/.config/kthcorpus:/root/.config/kthcorpus" \
		-v "$(pwd)/report:/tmp" \
			kthcorpus R -e "library(kthcorpus);checks_render_report()"
