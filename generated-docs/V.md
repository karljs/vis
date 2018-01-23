## Module V

#### `Dim`

``` purescript
type Dim = String
```

#### `V`

``` purescript
data V a
  = Chc Dim (V a) (V a)
  | One a
```


