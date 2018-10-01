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

# Commonly used browser objects

### window.location
Provides information about the current document URI. Can also be used to redirect the browser although with the browser history API this is becoming less common. We can parse this location and obtain the url query parameters. Like so:
```javascript
const getQueryParam = (key) => new URL(window.location.href).searchParams.get(key)
```
### window.navigation
    - Request users location (geolocation API)
    - Check if the browser is online or offline
    - Access the permissions API. This is used for displaying notifications to the user ect.
    - Access to users audio/video.
    - Obtain the current browser language.
    - Send beacon (useful for analytics)
    - Access to user clipboard
    - Access/register service worker
### window.isFullScreen
Check to see if the window is currently in full screen mode.
```javascript
window.isFullScreen?loadPrettyCss():loadUglyCss();
```
### window.devicePixelRatio
Returns the ratio of number of dots to CSS pixels. A dot is another name for a device independent pixel. This number comes in handy when you need to load images at specific sizes for user devices.
### window.localStorage
Persitant browser storage in a key value pair format. It is becoming more common to use local storage to store access tokens rather than the older method of using cookies.
### window.sessionStorage
Like session storage, but only exists for the lifetime of the tab. This information is volatile, once the tab is closed all information is lost.
### window.scrollX / window.scrollY
The current scroll position of the document on the x or y axis. Can be useful in some situations however the intersection observer API provides more performant solutions in most situations.
### window.Blob
Used to construct a blob of data. This technique comes in handy quite often. One example is creating a worker script at run time so that it runs off of the main browser thread.
### window.document


