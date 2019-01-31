# bundle exec jekyll build && htmlproofer --log-level ':debug' --assume-extension _site
bundle exec jekyll build && htmlproofer --alt-ignore "/style.anu.edu.au/" --file-ignore "/404/" --assume-extension _site
