# Servant adapters for Cloudflare Workers

## What is this?

This monorepo provides adapters for Servant to use with [Cloudflare Workers](https://www.cloudflare.com/developer-platform/products/workers/).
You can see example usage in [Humblr](https://github.com/konn/humblr) and its [corresponding Discourse Post](https://discourse.haskell.org/t/blog-system-on-cloudflare-workers-powered-by-servant-and-miso-using-ghc-wasm-backend).
It currently provides the following libraries:

- `servant-cloudflare-workers`: `servant-server` analogue for Cloudflare Workers for routing Cloudflare Workers.
- `servant-client-fetch`: `servant-client` which runs with JavaScript [Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API).
- [`servant-auth-lite`](./servant-auth/servant-auth-lite): Lightweight version of `servant-auth` which compiles with WASM backend.
  + [`servant-auth-lite-client`](./servant-auth/servant-auth-lite-client): Its implementation for `servant-client`.
  + [`servant-auth-lite-client`](./servant-auth/servant-auth-cloudflare-workers): Its implementation for `servant-cloudflare-workers`.
