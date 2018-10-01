---
title: Useful browser API's
date: 2018-10-01T00:33:54.996Z
url: useful_browser_api's
time_to_read: '5'
abstract: >-
  Browser API's can sometimes be hard to discover, especially when they change
  so often. Let's talk about some of the most useful and common browser API's
  available.
image: 3.jpeg
tags: 'browser,DOM,BOM,CSSOM,javascript'
---
Working in the browser can sometimes be daunting if you [look](https://developer.mozilla.org/kab/docs/Web/API) at the amount of API's available. Luckily, there is light at the end of the tunnel and this huge list can be narrowed down to the most frequently used and important API's.

## Commonly used browser objects
### window.location
Provides information about the current document URI. Can also be used to redirect the browser although with the browser history API this is becoming less common. One quite comon thing to do with window.location is obtain the url query parameters. Like so:
```javascript
const getQueryParam = (key) => new URL(window.location.href).searchParams.get('key')
```
### window.navigation

