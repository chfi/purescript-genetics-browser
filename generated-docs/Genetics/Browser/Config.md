## Module Genetics.Browser.Config

#### `BrowserConfig`

``` purescript
newtype BrowserConfig
  = BrowserConfig { wrapRenderer :: RenderWrapper, browser :: BrowserConstructor, tracks :: TracksMap }
```

The configuration type for the whole browser

#### `parseBrowserConfig`

``` purescript
parseBrowserConfig :: Foreign -> F BrowserConfig
```


