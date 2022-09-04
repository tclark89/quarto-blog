FROM httpd:2.4
COPY ./_site/ /usr/local/apache2/htdocs/
