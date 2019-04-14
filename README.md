### Building & running

```
./make
docker-compose up -d
cabal v2-run
```

### Coding conventions

- Effects are defined in `Hp.Eff.<EffectName>` and named
  `<EffectName>Effect`
- Carriers are defined in `Hp.Eff.<EffectName>.<CarrierName>` and named
  `<EffectName>Carrier<CarrierName>`
- Types with a persistent identity are defined in `Hp.Entity.<TypeName>` and
  implement the `IsEntity` type class
- Event types (domain events of interest to other parts of the application) are
  defined in `Hp.Event.<EventName>` and use the past tense
- Misc. types are defined in `Hp.<TypeName>`
