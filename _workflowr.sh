git add _workflowr.yml \
		    _workflowr.R \
		    _workflowr.sh

git add analysis/_site.yml \
        analysis/about.Rmd \
		    analysis/index.Rmd \
		    analysis/license.Rmd \
		    analysis/01-quality-control.Rmd \
		    analysis/02-normalization.Rmd \
		    analysis/03-feature-selection.Rmd \
		    analysis/04-reduced-dimensions.Rmd \
		    analysis/05-clustering.Rmd \
		    analysis/06-doublet-detection.Rmd \
		    analysis/07-cell-cycle.Rmd \
		    analysis/08-marker-detection.Rmd \
		    analysis/09-cell-annotation.Rmd \
		    analysis/10-atlas-integration.Rmd \
		    analysis/interactive.Rmd

git add docs/about.html \
		    docs/index.html \
		    docs/license.html \
		    docs/01-quality-control.html \
		    docs/02-normalization.html \
		    docs/03-feature-selection.html \
		    docs/04-reduced-dimensions.html \
		    docs/05-clustering.html \
		    docs/06-doublet-detection.html \
		    docs/07-cell-cycle.html \
		    docs/08-marker-detection.html \
		    docs/09-cell-annotation.html \
		    docs/10-atlas-integration.html \
		    docs/interactive.html

git add docs/figure/01-quality-control.Rmd \
		    docs/figure/02-normalization.Rmd \
		    docs/figure/03-feature-selection.Rmd \
		    docs/figure/04-reduced-dimensions.Rmd \
		    docs/figure/05-clustering.Rmd \
		    docs/figure/06-doublet-detection.Rmd \
		    docs/figure/07-cell-cycle.Rmd \
		    docs/figure/08-marker-detection.Rmd \
		    docs/figure/09-cell-annotation.Rmd \
		    docs/figure/10-atlas-integration.Rmd

git add docs/site_libs \
		    docs/.nojekyll

git commit -m "publish"

git push origin master
