LCR Proxy
=========

This is a simple app the proxy requests to LCR.

### Why?

At the moment, `lds.org` doesn't support CORS. I want to build a simple webpage where I can provider my LDS Account credentials, and then get a list of email addresses for the members of our Elder's Quorum. Everything I need to do could be done on the frontend, if `lds.org` supported CORS. Since they don't, I'm creating this service which _will_ support CORS, and then send the request to `lds.org` on the backend.

### Local Testing

`foreman start` will start local server.

### Deployment

This app deploys to Heroku.

1. Create the app. From the root project directory:

```
heroku create --buildpack http://github.com/drautb/heroku-buildpack-racket.git lcr-proxy
```

2. Make sure that a Procfile exists in the project root.

```
web: ./racketapp
```

3. Configure the environment.

```
heroku config:set PLTSTDERR=info
```

4. Ship it.

```
git push heroku master
```

Powered by [Racket][1].

[1]: http://racket-lang.org/
