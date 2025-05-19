--newPackage(
--    "MRDIJSON",
--    Headline => "MRDI file format",
--    Version => "0.1",
--    Date => "November 25, 2024",
--    Authors => {{
--	    Name => "Antony Della Vecchia",
--	    Email => "vecchia@math.tu-berlin.de",
--	    HomePage => "https://antonydellavecchia.github.io"}},
--    Keywords => {"System"},
--    PackageImports => {"JSON"},
--    AuxiliaryFiles => true)
--
--export {"saveMRDIJSON"}
needsPackage "JSON"

-- serializable types
-- {type, name of type in file, uses Id}
typeInfo = {
    {PolynomialRing, "MPolyRing", true}
    }

reverseTypeMap = new HashTable {
    "MPolyRing" => PolynomialRing,
    "PolyRing" => PolynomialRing
}

typeParams = method()
typeParams PolynomialRing := (obj) -> (
    return (PolynomialRing, baseRing(obj))
    )

typeParams Ring := (obj) -> (
    if hash(obj) == hash(QQ) then return (Ring, null);
    if hash(obj) == hash(ZZ) then return (Ring, null);
    return (Ring, baseRing(obj));
    )

typeParams RingElement := (obj) -> (
    return (RingElement, ring(obj));
    );


saveTypeParams = method()
saveTypeParams(MutableHashTable, Thing) := (state,  obj) -> (
    "MPolyRing"
    );

saveTypeParams(MutableHashTable, RingElement) := (state,  obj) -> (
    new HashTable from {
	"name" => "MPolyRingElem",
	"params" => hash ring obj
	}
    );

saveObject = method()
saveObject(MutableHashTable, PolynomialRing) := (state, obj) -> (
    new HashTable from {
	"base_ring" => new HashTable from {"_type" => "QQField"},
	"symbols" => toString \ gens(obj)
	}
    )

saveObject(MutableHashTable, RingElement) := (state, obj) -> (
    listForm obj 
)


saveTypedObject = method()
saveTypedObject(MutableHashTable, Thing) := (state, obj) -> (
    state#"objToId"#(class obj) ??= hash class obj;
    state#"idToObj"#(hash class obj) ??= class obj;
    
    h := new HashTable from {
	"_type" => saveTypeParams(state, obj),
	"data" => saveObject(state, obj)
	};
	
    )

saveMRDIJSON = method()
saveMRDIJSON(String, Thing) := (filename, obj) -> (
    -- this should become a global serializer state to save many objects in the
    -- same session
    state := new MutableHashTable from {
	"objToId" => new MutableHashTable,
	"idToObj" => new MutableHashTable
	};

    objAsDict := saveTypedObject(state, obj)
    
    -- write objAsDict to a file
    );

stringIsUUID = s -> (
    --  A simple check: 36 characters, and some hyphens in specific positions
    --  This is NOT a fully compliant UUID validation, but it's sufficient
    --  for the purpose of this translation, given the limitations of
    --  Macaulay2's built-in string manipulation.  A more robust check
    --  would require more complex string parsing.
    (length s == 36) and
    (s_(9) == "-") and (s_(14) == "-") and (s_(19) == "-") and (s_(24) == "-")
);

-- The decode_type function in Macaulay2
decodeType = s -> (
    if (type(s#obj) == String) then (
        if (stringIsUUID(s#obj)) then (
            id := s#obj;
            obj := s#obj;
            if (s#refs == null) then (
                return type(s#idToObj#id);
            );
            s#obj = s#refs#id;
            T := decodeType(s);
            s#obj = obj;
            return T;
        ) else (
            key := s#obj;
            if (key in reverseTypeMap) then (
              return reverseTypeMap#key
            ) else (
              error("unsupported type " | key);
            )
        )
    );

    if ("_type" in keys(s#obj)) then (
        --- need to work out loadNode 
        return decodeTypes#obj#"_type") => decodeType(s)
    );

    if ("name" in keys(s#obj)) then (
        if ("_instance" in keys(s#obj)) then (
            nameKey := s#obj#"name";
            instanceKey := s#obj#"_instance";
            if (instanceKey in reverseTypeMap#nameKey) then (
               return reverseTypeMap#nameKey#instanceKey
            ) else (
              error("unsupported instance " | instanceKey);
            )
        ) else (
            return decodeType(s#"name");
        )
    )
);

loadTypedObject = state -> (
    	T = decodeType(state);
    );

loadMRDIJSON = filename -> (
    -- this should become a global serializer state to save many objects in the
    -- same session
    jsonHT = fromJSON openIn filename
    state := new MutableHashTable from {
	"objToId" => new MutableHashTable,
	"idToObj" => new MutableHashTable,
	"obj" => jsonHT,
	"refs" => jsonHT#"_refs"
	};
    return loadTypedObject(state);
    );



--end--
--restart
--needs "MRDIJSON.m2"

loadMRDIJSON("/homes/combi/vecchia/local/repositories/mardi/file-format-paper/polynomial-example.json")





