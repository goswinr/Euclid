module internal JsonConvert

open System
()

#if !FABLE_COMPILER
open System.Text.Json


let private jsonPropertyName (options:JsonSerializerOptions) name =
    match options.PropertyNamingPolicy with
    | null -> name
    | namingPolicy -> namingPolicy.ConvertName(name)


/// Writes an object represented by equally sized arrays of property names and float values.
let writeFloatProperties
    (writer:Utf8JsonWriter)
    (options:JsonSerializerOptions)
    (typeName:string)
    (names:string[])
    (values:float[]) =
        if values.Length <> names.Length then
            invalidOp $"The JSON converter for {typeName} received {values.Length} values for {names.Length} names."

        writer.WriteStartObject()
        for i = 0 to names.Length - 1 do
            writer.WriteNumber(jsonPropertyName options names.[i], values.[i])
        writer.WriteEndObject()


/// Reads an object into float values ordered like the supplied property names.
let readFloatProperties
    (reader:byref<Utf8JsonReader>)
    (options:JsonSerializerOptions)
    (typeName:string)
    (names:string[]) =
        if reader.TokenType <> JsonTokenType.StartObject then
            raise (JsonException $"Expected a JSON object for {typeName}.")

        let jsonNames = names |> Array.map (jsonPropertyName options)
        let comparison =
            if options.PropertyNameCaseInsensitive then
                StringComparison.OrdinalIgnoreCase
            else
                StringComparison.Ordinal

        let values = Array.zeroCreate<float> names.Length
        let mutable complete = false

        while not complete && reader.Read() do
            match reader.TokenType with
            | JsonTokenType.EndObject ->
                complete <- true
            | JsonTokenType.PropertyName ->
                let name = reader.GetString()
                if not (reader.Read()) then
                    raise (JsonException $"Unexpected end of JSON while reading {typeName}.")

                match jsonNames |> Array.tryFindIndex (fun expected -> String.Equals(name, expected, comparison)) with
                | Some index ->
                    values.[index] <- reader.GetDouble()
                | None ->
                    reader.Skip()
            | _ ->
                raise (JsonException $"Expected a property name while reading {typeName}.")

        if not complete then
            raise (JsonException $"Unexpected end of JSON while reading {typeName}.")

        values


#endif