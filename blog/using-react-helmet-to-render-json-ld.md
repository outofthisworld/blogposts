---
title: Using react-helmet to render JSON-LD
date: 2018-10-01T00:02:01.562Z
url: rendering_json-ld_using_react_helmet
time_to_read: '4'
abstract: >-
  React helmet is a library built for react allowing you to inject headers into
  the head of your HTML. It allows you to easily dynamically configure your HEAD
  section in a react app at runtime.
image: 4.jpeg
tags: 'react16,react helmet,json-ld,javascript'
---

React helmet is a library built for react allowing you to inject headers into the head of your HTML. It allows you to easily dynamically configure your HEAD section in a react app at run-time.
The best part is, react-helmet supports server-sided rendering. As such, it works perfect for rendering JSON-LD. Although google's web crawlers are said to be able to read dynamically generated JSON-LD, it is not clear whether or not this is actually the case.

## Installing react-helmet
You can install react-helmet via npm using 
```bash
npm install --save react-helmet
```

## Dynamically generating JSON-LD
Once you have react-helmet installed, you can straight away get to configuring the JSON-LD.

A simple structure might look like this:
```javascript
<script type="application/ld+json">
{
  {
    "@context": "http://schema.org/",
    "@type": "WebSite",
    "name": "${data.site.title}",
    "url": "${data.site.url}",
    "potentialAction": {
        "@type": "SearchAction",
        "target": "${data.site.url}/search?search={search_term_string}",
    "query-input": "required name=search_term_string"
    }
  }
}
</script>
```
The above snippet is a snippet taken from this website describing what the website is about and informs search engines how search can be performed on the site. This allows them to display a search bar when your website appears in search results. The search bar is configured to use your sites search functionality directly, which is a great thing for users and the overall user experience. 

In the above example,  `@context` describes the schema context that the JSON-LD structure is using. `@type` defines the type from the schema that you are describing. There are many different types available from http://schema.org for example, heres some JSON-LD to describe a blog post.

```javascript
<script type="application/ld+json">
{
  {
    "@context": "http://schema.org",
    "@type": "Article",
    "headline": ${this.props.data.markdownRemark.frontmatter.abstract.slice(0,this.props.data.markdownRemark.frontmatter.abstract.indexOf('.')+1)},
    "image": {
        "@type": "ImageObject",
        "url": https://www.dalesdev.tech${this.props.data.file.childImageSharp.fluid.src},                                       
        "width":${this.props.data.file.childImageSharp.fluid.presentationWidth},
        "height": ${this.props.data.file.childImageSharp.fluid.presentationHeight}
    },
    "description": ${this.props.data.markdownRemark.frontmatter.abstract},
    "datePublished": ${this.props.data.markdownRemark.frontmatter.date},
    "author": {
        "@type": "Person",
        "name": "Dale Appleby"
    },     
  }
}
</script>
```

### Rendering with react-helmet
