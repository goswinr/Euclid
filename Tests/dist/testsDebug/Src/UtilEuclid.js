
import { Attribute } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { sign } from "../fable_modules/fable-library-js.5.0.0/Util.js";

export class System_Runtime_Serialization_DataContract extends Attribute {
    constructor() {
        super();
    }
}

export function System_Runtime_Serialization_DataContract_$reflection() {
    return class_type("System.Runtime.Serialization.DataContract", undefined, System_Runtime_Serialization_DataContract, class_type("System.Attribute"));
}

export function System_Runtime_Serialization_DataContract_$ctor() {
    return new System_Runtime_Serialization_DataContract();
}

export class System_Runtime_Serialization_DataMember extends Attribute {
    constructor() {
        super();
    }
}

export function System_Runtime_Serialization_DataMember_$reflection() {
    return class_type("System.Runtime.Serialization.DataMember", undefined, System_Runtime_Serialization_DataMember, class_type("System.Attribute"));
}

export function System_Runtime_Serialization_DataMember_$ctor() {
    return new System_Runtime_Serialization_DataMember();
}

/**
 * Match the sign (+ or -) to a given number.
 */
export function Euclid_UtilEuclid_matchSign(signedValue, numToMatch) {
    if (sign(signedValue) === sign(numToMatch)) {
        return numToMatch;
    }
    else {
        return -numToMatch;
    }
}

/**
 * Converts a cosine value to degrees.
 */
export function Euclid_Cosine_inDegrees(c) {
    return 57.29577951308232 * Math.acos(c);
}

/**
 * Converts a tangent value to degrees.
 */
export function Euclid_Tangent_inDegrees(t) {
    return 57.29577951308232 * Math.atan(t);
}

