# quarto-blog
A blog created with Quarto and RStudio

Also contained in the repository are:
 - A Dockerfile for building a modified httpd image
    - This dockerfile copies the docs directory to the httpd directory
      and would need to be rebuilt with every change
 - A docker-compose.yml file for spinning up a httpd image
    - The compose file links the docs directory to the httpd directory
      so changes would be reflected realtime and no image needs to be built
