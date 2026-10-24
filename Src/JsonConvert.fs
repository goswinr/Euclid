module internal JsonConvert

open System
()

#if !FABLE_COMPILER
open System.Text.Json

// The converters read the properties strictly in the order they write them.
// So each converter's Read mirrors its Write, and reading needs no lookup or buffer.


let private jsonPropertyName (options:JsonSerializerOptions) name =
    match options.PropertyNamingPolicy with
    | null -> name
    | namingPolicy -> namingPolicy.ConvertName(name)


/// Writes a float property, with its name converted by the naming policy of the options.
let writeFloat
    (writer:Utf8JsonWriter)
    (options:JsonSerializerOptions)
    (name:string)
    (value:float) =
        writer.WriteNumber(jsonPropertyName options name, value)


/// Writes a property holding the floats of an array as a JSON array.
let writeFloatArray
    (writer:Utf8JsonWriter)
    (options:JsonSerializerOptions)
    (name:string)
    (xs:float[]) =
        writer.WriteStartArray(jsonPropertyName options name)
        for i = 0 to xs.Length - 1 do
            writer.WriteNumberValue(xs.[i])
        writer.WriteEndArray()


/// Writes a property holding the floats of a ResizeArray as a JSON array.
let writeFloatResizeArray
    (writer:Utf8JsonWriter)
    (options:JsonSerializerOptions)
    (name:string)
    (xs:ResizeArray<float>) =
        writer.WriteStartArray(jsonPropertyName options name)
        for i = 0 to xs.Count - 1 do
            writer.WriteNumberValue(xs.[i])
        writer.WriteEndArray()


/// Checks that the reader is on the start of a JSON object, where System.Text.Json places it before it calls a converter.
let readStartObject
    (reader:byref<Utf8JsonReader>)
    (typeName:string) =
        if reader.TokenType <> JsonTokenType.StartObject then
            raise (JsonException $"Expected a JSON object for {typeName}.")


/// Reads the end of the JSON object. Fails if the object has more properties.
let readEndObject
    (reader:byref<Utf8JsonReader>)
    (typeName:string) =
        if not (reader.Read()) || reader.TokenType <> JsonTokenType.EndObject then
            raise (JsonException $"Expected the end of the JSON object for {typeName}.")


/// Reads the next property name, which must be the given name, and moves the reader to its value.
let private readPropertyName
    (reader:byref<Utf8JsonReader>)
    (options:JsonSerializerOptions)
    (typeName:string)
    (name:string) =
        let jsonName = jsonPropertyName options name
        if not (reader.Read()) || reader.TokenType <> JsonTokenType.PropertyName then
            raise (JsonException $"Expected the property {jsonName} while reading {typeName}.")

        // Only a name that differs from the written one needs to be compared as a string.
        let isMatch =
            reader.ValueTextEquals(jsonName)
            || options.PropertyNameCaseInsensitive && String.Equals(reader.GetString(), jsonName, StringComparison.OrdinalIgnoreCase)
        if not isMatch then
            raise (JsonException $"Expected the property {jsonName} while reading {typeName}, but found {reader.GetString()}.")

        if not (reader.Read()) then
            raise (JsonException $"Unexpected end of JSON while reading {typeName}.")


/// Reads the next property, which must have the given name, and returns its float value.
let readFloat
    (reader:byref<Utf8JsonReader>)
    (options:JsonSerializerOptions)
    (typeName:string)
    (name:string) : float =
        readPropertyName &reader options typeName name
        if reader.TokenType <> JsonTokenType.Number then
            raise (JsonException $"Expected a number for the property {name} while reading {typeName}.")
        reader.GetDouble()


/// Returns the number of floats in the JSON array the reader is on, without moving the reader.
/// It reads ahead on a copy of the reader, since System.Text.Json buffers the whole value before it calls a converter.
let private countFloats
    (reader:byref<Utf8JsonReader>)
    (typeName:string) =
        if reader.TokenType <> JsonTokenType.StartArray then
            raise (JsonException $"Expected a JSON array of numbers for {typeName}.")

        let mutable ahead = reader
        let mutable count = 0
        while ahead.Read() && ahead.TokenType = JsonTokenType.Number do
            count <- count + 1

        if ahead.TokenType <> JsonTokenType.EndArray then
            raise (JsonException $"Expected a JSON array of numbers for {typeName}.")

        count


/// Reads the next property, which must have the given name and hold a JSON array of floats,
/// into a new array of exactly that length.
let readFloatArray
    (reader:byref<Utf8JsonReader>)
    (options:JsonSerializerOptions)
    (typeName:string)
    (name:string) : float[] =
        readPropertyName &reader options typeName name
        let count = countFloats &reader typeName
        let xs = Array.zeroCreate count
        for i = 0 to count - 1 do
            reader.Read() |> ignore
            xs.[i] <- reader.GetDouble()
        reader.Read() |> ignore // to the EndArray token
        xs


/// Reads the next property, which must have the given name and hold a JSON array of floats,
/// into a new ResizeArray with a capacity of exactly that length.
let readFloatResizeArray
    (reader:byref<Utf8JsonReader>)
    (options:JsonSerializerOptions)
    (typeName:string)
    (name:string) : ResizeArray<float> =
        readPropertyName &reader options typeName name
        let count = countFloats &reader typeName
        let xs = ResizeArray<float>(count)
        for _ = 1 to count do
            reader.Read() |> ignore
            xs.Add(reader.GetDouble())
        reader.Read() |> ignore // to the EndArray token
        xs


#endif
