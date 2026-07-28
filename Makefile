.PHONY: setup demo run test lint docker-up docker-down docker-logs docker-build

setup:
	Rscript scripts/install_packages.R

demo:
	Rscript scripts/generate_demo_data.R

run:
	Rscript scripts/run_app.R

test:
	LLM_MODE=mock Rscript -e "testthat::test_dir('tests/testthat')"

lint:
	Rscript -e "if (!requireNamespace('lintr', quietly = TRUE)) install.packages('lintr', repos = 'https://cloud.r-project.org'); lintr::lint_dir('R')"

docker-build:
	docker compose build

docker-up:
	docker compose up --build

docker-down:
	docker compose down

docker-logs:
	docker compose logs -f app
