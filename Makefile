.PHONY: test bench

build: clean format-app test-app

format-app:
	cd app && for path in $$(ls -d *.cabal); do cabal-fmt -c $$path || cabal-fmt -i $$path; done
	cd app && ormolu -ci $$(find . -name "*.hs" -not -path "./*.stack-work/*" -not -path "./dist/*" -not -path "./dist-newstyle/*" -not -path "./.git/*")

test-app:
	cd app && stack test --fast --ghc-options="-Werror -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints -Wunused-packages -Wno-name-shadowing" +RTS -A128m -n2m -N -RTS

clean:
	rm -rf app/dist
