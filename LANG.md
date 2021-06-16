KV Language

* In the face of ambiguity, blow up.
* Constantly filter down a set until there is only one thing left.

Json = Just represent the objects as KV pairs because they already are.

HTML = Every object has a key for the tag type and every attribute in the tag
(class, id, etc.). Attributes on a tag are also Values that can be explored.

PDL:
`{"figure": "image"}`


Weather:
```
{
    "currently": {
        "summary=currently",
        "apparentTemperature=currentTemp",
    }
    "daily": {
        "summary=summary",
        "data": {
            "0=dayData"
        }
    }
}
```
