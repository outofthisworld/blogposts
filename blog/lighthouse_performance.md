---
title: Aiming for the perfect lighthouse score
date: '2018-04-10'
url: perfect_lighthouse_score
time_to_read: '12'
abstract: >-
  Lighthouse is handy, and I want to clock it. Lighthouse is a website auditing
  tool baked into google chrome that can help with accessing your web apps
  performance. You can use it to simulate slow 3g networks by and also run your
  web app on a simulated mobile device with a slower cpu. It provides some
  decent metrics and performance indicators showing you where you can improve
  you web app.
image: 2.jpeg
tags: 'react,web development,react16,javascript'
---
# Lighthouse

Lighthouse is a website auditing tool baked into google chrome that can help
with accessing your web apps performance. You can use it to simulate slow 3g networks by and also run your web app on a simulated mobile device with a slower cpu. It provides some decent metrics and performance indicators showing you where you can improve you web app.

<br/>

I have ran audits a few times during the development of this website and I'm pretty happy with the results:

![Lighthouse performance metrics](./blog_assets/lighthouse.png "Lighhouse performance metrics")

I think it's looking pretty good! There are some places to improve, notably in the accessibility area. Right now it's being degraded as a few elements are not nested correctly which is necessary screen readers, for example a `<li>` inside a `<div>` is a no go. Some of them are beyond my control, for example assets not being served via HTTP/2. 

This website is statically generated and served via a third party server so unfortunetly I have no control over the HTTP protocol they use. If/when they upgrade to HTTP/2 it should definitely show some drastic perforance increases though, due to the server being able to push static assets before the browser even knows it needs them 😀😀😀

So what steps do you have to take in order to improve you lighthouse score?

The perfect lighthouse score is essentially centered around 5 main areas. Performance,
Progressive Web App, Accessibility, Best practices and SEO. However, these can be broken down into subcategories that will drastically improve your rating:

### 1) Code splitting

Instead of one big bundle containing javascript that a page doesn't use, have multiple smaller bundles and preload/prefetch/push these smaller bundles based upon whether or not the user is likely to need them during the current navigation.

Code splitting is one of the most compelling features of webpack. This feature allows you to split your code into various bundles which can then be loaded on demand or in parallel. It can be used to achieve smaller bundles and control resource load prioritization which, if used correctly, can have a major impact on load time.

* Extracting common code to each bundle out to a seperate bundle (e.g both bundles use lodash so lodash should be bundled seperately)

### 2) Lazy loading javascript (on demand loading)

Conditionally load javascript when it is needed and make the chunk prefetch or preload.

### 2) Dead code elimination (Tree shaking)

Remove ununsed code from a bundle. This is a given, if it's not used, the user doesn't need to download it. Many build tools include this option, for example Webpack.

### 3) Minify files

Minify javascript and css by trimming whitespace and compacting it. Essentially compressing the code without obfuscating it.

### 4) HTTP/2 server push

Allows a server to push assets to the client before they have been requested. This is to stop the need for inline css/js which increases a websites percieved load time and are sort of anti patterns. The idea is that a Link preload header can be used to tell the browser that the following resource will be needed for the current navigation. Push critical assets (e.g css) before the client actually requests them. Some servers automatically do this when a Link header is included in the response, however the concept of server push and preload are different in nature.

The server begins sending a file to the browser before it even asks for it. For example if style.css and bundle.js is required by index.html, we can push style.css and bundle.js

at the same time as the index.html request. This means that before the HTML gets back to the browser, the browser has already begun downloading the other scripts/css required by the page.

### 5) Preload and prefetch

The browser is aware of a resource before it has recognized it needs it during parsing.

The resource has begun downloading and by the time, for example, the CSS font or image is required in the CSS, it has potentially already been downloaded or is partially completed downloading because of the preload. 

This can be very useful especially if the CSS file is large and parsing could take a little. Its basically a list of resources that are going to be needed, but the browser is aware before interpreting any code. It takes your word that this resource is going to be needed and downloads it.

* Preload only necessary resources that will be needed for the current page
* Do testing to ensure performance is actually increased
* Make sure to include the `type` and `as` attributes if in head of html document

### 6) Service worker

Use a service worker to cache resources and common requests. Service workers can become complicated and there are some tools that will generate them for you based on your site. There are also some gotchas to watch out for, such as accidently downloading the same resource twice. Be vigiliant and aware, however will undoubtedly increase any websites performance.

If you are using webpack you can use the Workbox plugin, which makes it easier to create service workers for your app.

### 7) Use HTTP/2

HTTP/2 is the new version of the HTTP protocol. It is more efficient as it uses the same TCP connection accross requests removing the need for multiple TCP handshakes speeding up the network. It does this uses the concept of streams and frames. HTTP/2 introduces many new features which as developers we can take advtange of. One such feature is HTTP/2 server push
which allows the server to push assets to the client without the client even asking for them! 

### 8) HTTP cache using cache-control header

Cache static resources like css/js. Use content hash or versioning to make sure the client has the most up to date versions of these resources.

### 9) Inline, in the absence of HTTP/2 and server push

Inline styles/javascript. Why? because it decreases initial page load time. It's a trade off, either the first ever visit to your website is fast and subsequent visits are slower (inline styles/javascript are not cached) or the first visit is slow and subsequent visits are faster (with appropriate cacheing of the external resources). Prefer preload/prefetch, HTTP/2 server push and service worker when possible. Note that inline scripts and styles also reduce security. With external files you can specify a `content-security-policy` which also stops any injected inline scripts on your page from running.

### 10) Use compression

Use some sort of compression on the server side for large files.

### 11) Statically build your website

There's a trend moving towards statically building your website and it is great for two reasons. One, is that you get to use modern web development techniques with the benefit of SEO. The second is because it's easy to scale and removes the need for users to wait for content to be available while spinners are shown, decreasing load time and increasing performance. The idea is that the site is statically generated, and an update to the underlying websites data source (CRM, database, github, whatever) triggers a event which notifies your build pipeline to pull in these changes and re-deploy. The site acts like it's dynamic but really it's static. This works well for javascript libraries like React, but not necessary if you are using a templating engine like Handlebars/Jade/Pug on the server side.

## Browser loading scenarios; sequential, preload, preload+HTTP/2 server push.

Heres a breakdown of page loading scenarios in the browser, from using the traditional sequential method to using preload and HTTP/2 server push.

#### Sequential loading:

1. Load: index.html
2. Load: style.css
3. Parse css
4. Load: myneededfont.wof (required in css file)
5. Load: myneededimage.png (required in css file)
6. All done

#### With preload

1. Load: index.html
2. Load: style.css
3. Preload: myneededfont.wof preload: myneededimage.png (at the same time)
4. Parse: style.css
5. Fetch from cache: myneededfont.wof (already downloaded due to preload)
6. Fetch from cache: myneededimage.png (already downloaded due to preload)
7. All done

#### With Http/2 server push and preload:

1. Load: index.html
2. Server push: style.css,myneededfont.wof,myneededimage.png
3. Preload: myneededfont.wof preload: myneededimage.png (at the same time)
4. Load remaining: style.css (May already be downloaded/cached due to server push)
5. Parse: style.css
6. Fetch from cache: myneededfont.wof (already downloaded due to preload)
7. Fetch from cache: myneededimage.png (already downloaded due to preload)
8. All done

#### Take away:

* The number of steps does not correlate to the speed. Actually as shown above it is quite the opposite, the fastest option requires the most steps. 
* Loading sequentially is the least performant. Everything is done in order and has to wait for the previous step to finish. Nothing can be done asynchronously. There is no parallelism and no concurrency. It can be compared to the waterfall software development methodology which has long been abandoned for all but the most trivial software projects.
* Including preload links can be helpful for the browser as it is aware of resources it needs ahead of time. As such, downloads are triggered pre-emptively - before the browser knows they are required. When they are required, the browser already has some if not all of the resource ready waiting. You can assume that page load time will decrease by the amount of time it takes for the browser to recongnize that the resource is required. Because of this, larger html pages/css files and javscript files will benefit the most while others will see smaller (but still some) performance benefits.
* HTTP/2 server push can help with round trip latency between the client and the server and solves a different problem to link preloading. Without HTTP/2 server push, the browser requests index.html, downloads & parses it, discovers it needs style.css and then begins downloading it. As such, the browser requests style.css, downloads it, parses it and discovers it needs myneededimage.png. Super advanced right?. Really it's quite primitive and is long overdue for an overhaul. Now we can push style.css at the same time as index.html, without the browser first initially parsing index.html and making a seperate request for style.css. Thus, potentially, we have saved the browser from a whole round trip to our server to obtain style.css. I say potentially because the client at any time can refuse the pushed resource.

## Performance checklist

* HTTP/2 push any main files that are definitely needed for that page. (index.chunk.js and style.css or index.style.css, so that they don't have to be requested once the HTML is parsed). Push resources you would traditionally inline, such as small images, CSS and JS. You can leverage the browser cache better when you refer to it as a separate asset. When you inline, you are unnecessarily sending the asset along with the HTML every time.
* Preload any resources required by these files (somefont.wof, someimage.png)
* Preload any conditional resources required on that same page (for instance if a user clicks a button some script is required that does something) .These are resources that are very likely required for the current navigation, but potentially wont be hit (oops the user didn't click that button which triggers that javascript resource); that's a cache miss for ya.
* Prefetch files required in a different page that is likely to be visited from this page.

How do you know which files to prefetch? Anayltics, intuition, guess work at first really. Don't prefetch to much, it'll degrade performance and eat up the users bandwidth which could deter them from coming to your site again.

### That's a lot. Maybe a checklist can help for progressive enhancment:

* Website has an appropriate cache-control header on ALL OF critical static resources.
* Websites critical resources and network requests being cached by a service worker.
* Websites preloads any files that might be needed before CSS parsing/JS parsing completes.
* Website has preloaded any files needed for the current navigation.
* Website prefetched any files needed for a probable navigation.
* Website uses HTTP/2 server push on any resources needed before HTML parsing completes. All css files, all JS files and potentially some images on the page. (Note you can also push fonts/images that are preloaded, there is no one stop shop)
* Website javascript split into different bundles, based on page/route or some other optimal strategy for determining how bundles should be split.
* Website has minified javascript, and is optimised by a transpiler
  for the browsers it should support (use babel).
* Website has stripped and minified my html.
* Website has vendor prefixed CSS (auto-prefixer) and had the css optimized.
* Website images are optimised for different screen sizes (this is something important that is often overlooked but can drastically increase page load times). Prefer SVG where possible. Use picture/source src-set for raster based images.

### Testing performance.

So you've completed your checklist? What's left? Well now you need to test how performant it really is. How do you do that, what's performant? What's not? How do you benchmark a website?

Well, there are many tools available to test a websites performance and the methods used depend on the web application in question and where the bottlenecks are likely to be. However EVERY web app requires the same basic measurments (after each of these ask: can I decrease this in some way?) :

* Website load time, latency.
* Size of files downloaded 
* Amount of files requested 
* Time to render 
* Time to load DOM
* Time for DNS to resolve
* Time for javascript to execute
* Time for css to parse

Defering some unimportant scripts can help increase the page load speed. You can also make sure that files are being compressed on the sever side to speed up transfer time.

Examing your browser developer tools. Make use of those statistics. Figure out how your website is functioning. Visit different pages, do all these pages require all the javascript that is loaded on the page? No? remove it. Does it need to execute straight away? No? defer it. Can it be done out of page order? make it async. The best thing is to be inquistive and ask tons of questions, why? when? who? what? where? how? Ask who so you can blame the person that did it. No that's probably not appropriate, kindly inform them of a better way - they will thank you!  :)

Lets continue, (how can I increase this in some way)

* Number of files cached by service worker
* Number of critical resources in HTTP cache
* Number of critical resources sent with HTTP/2 push
* Number of network/fetches cached by service worker

How do you know what target to meet? 

* Find similar websites and measure and beat their load and rendering times. Remember if you can beat similar websites performance, your site has the edge when it comes to SEO (faster websites are given merits according to google https://developers.google.com/web/updates/2018/07/search-ads-speed)

# Tools to test speed:

* PageSpeed Insights, an online tool that shows speed field data for your site, alongside suggestions for common optimizations to improve it.
* Lighthouse, a lab tool providing personalized advice on how to improve your website across performance, accessibility, PWA, SEO, and other best practices.

(In browser Perfomance API)
//A script, first paint, first-contentful-paint
 if("performance" in window){
  window.addEventListener("load", ()=>{
    let paintMetrics = performance.getEntriesByType("paint");

```
if(paintMetrics !== undefined && paintMetrics.length > 0){
  paintMetrics.forEach((paintMetric)=>{
    console.log(`${paintMetric.name}: ${paintMetric.startTime}`);
  });
}
```

  });
}

https://developer.mozilla.org/en-US/docs/Web/API/Resource_Timing_API/Using_the_Resource_Timing_API
https://developer.mozilla.org/en-US/docs/Web/API/User_Timing_API/Using_the_User_Timing_API

* Look at statistics relating to how users act at different percieved web app network speeds and load times. Use this as an indication as to where your application should be falling.
* Another option is to use RAIL. A performance model based around the user-expierience. With this model, your performance goals should be centered on the user. Rail stands for response, animation, idle, load. According to this performance model, these are the main life cycles of every website.
  ```
    Heres a brief overview of what RAIL encompasses:
    - Users are the focal point
    - Process user input events in under 50ms and have a visible response ready
      within 100ms. This applies to must user input events but does not include drags/scrolls.
    - If processing user input events under 50ms is not possible, provide some form 
      of feedback.
    - Animate at 60 FPS or above. Aim for producing a frame every 10ms. Animations
      sould be smooth and crisp. Users notice when they are not. Use animation
      optimization stratergies to increase your animations performance.
    - Make use of idle time for processing. Use idle time for deffered work, 
      with any user actions during this time taking priority.
    - Make sure your websites loads in less than 5 seconds on a mid range celluar
      device on a slow 3G connection. Loads there after should be done in under 2 seconds.
    - Optimize the critical rendering path
    - Progressive rendering can be used to give the perception of a complete load,
      tasks that take longer can be deffered or done during idle time. 
      Check out requestIdleCallback for performing tasks while the user is inactive.
    - Be aware of the different areas that affect your page: Network, Hardware, Cache L1 L2,
      Parsing JS.
  ```

What can I do in dev tools to help test performance?

* Throttle your CPU to simulate a less-powerful device.
* Throttle the network to simulate slower connections.
* View main thread activity to view every event that occurred on the main thread while you were recording.
* View main thread activities in a table to sort activities based on which ones took up the most time.
* Analyze frames per second (FPS) to measure whether your animations truly run smoothly.
* Monitor CPU usage, JS heap size, DOM nodes, layouts per second, and more in real-time with the Performance Monitor.
* Visualize network requests that occurred while you were recording with the Network section.
* Capture screenshots while recording to play back exactly how the page looked while the page loaded, or an animation fired, and so on.
* View interactions to quickly identify what happened on a page after a user interacted with it.
* Find scroll performance issues in real-time by highlighting the page whenever a potentially problematic listener fires.
* View paint events in real-time to identify costly paint events that may be harming the performance of your animations.

## What other tools can I use?

* Lighthouse
* WebPageTest webpagetest.org/easy
