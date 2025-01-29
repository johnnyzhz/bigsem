---
title: Web application
layout: home
nav_order: 3
parent: Developer manual
---

The online app BigSEM uses R in the backend to run analyses on a web server. The current server setup for the working BigSEM app is below:

- Ubuntu 24.04.1 LTS
- <span class="s1">Apache/2.4.58</span>
- <span class="s1">PHP 8.3.6</span>
- <span class="s1">MySQL 8.0.40</span>
- <span class="s1">R 4.4.2</span>

<span class="s1">However, any server that supports PHP and MySQL should work. The app can also be easily set up on a local or remote server using [xampp](https://www.apachefriends.org/). If you plan to use the app offline, [xampp](https://www.apachefriends.org/) is highly recommended.</span>

## Graphical Interface 

![netapp3.png](https://bigsem.psychstat.org/manual/uploads/images/gallery/2024-10/scaled-1680-/2reFGiLC2duE6DKw-netapp3.png)

The graphical interface is developed using JavaScript. The core code is available at [https://github.com/johnnyzhz/semdiag](https://github.com/johnnyzhz/semdiag).

## R Setup 

One needs to install R and the networksem and TextSEM on the server first. R can be a security risk. If used on a remote server, some security measures should be taken. On our server, we use AppArmor and R package [RAppArmor](https://github.com/jeroen/RAppArmor).

## PHP for Bridging the Interface and R 

 We use PHP to connect to R to conduct the analysis.
