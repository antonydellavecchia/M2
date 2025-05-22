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
needsPackage "JSON" -- for writing/loading files to/from files 

-- serializable types
-- {type, name of type in file, uses Id}
typeInfo = {
    {PolynomialRing, "MPolyRing", true}
    }

reverseTypeMap = new HashTable from {
    "MPolyRing" => PolynomialRing,
    "PolyRing" => PolynomialRing, 
    "MPolyDecRing" => PolynomialRing,
    "MPolyRingElem" => RingElement,
    "PolyRingElem" => RingElement,
    "MPolyDecRingElem" => RingElement,
    "QQField" => Ring,
    "ZZRing" => Ring,
    "Vector" => List,
    "FinGenAbGroup" => null --ignore this type for now
}

singletonTypes = new HashTable from {
    "QQField" => QQ, 
    "ZZRing" => ZZ
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


-- saveTypedObject = method()
--saveTypedObject(MutableHashTable, Thing) := (state, obj) -> (
--    state#"objToId"#(class obj) ??= hash class obj;
--    state#"idToObj"#(hash class obj) ??= class obj;
--    
--    h := new HashTable from {
--	"_type" => saveTypeParams(state, obj),
--	"data" => saveObject(state, obj)
--	};
--	
--    )
--
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

isUUID = method()
isUUID String := s -> (
    -- Simple regex-like check for UUID format
    if #s != 36 then return false;
    if s#8 != "-" or s#13 != "-" or s#18 != "-" or s#23 != "-" then return false;
    
    validChars := set join(characters("0123456789abcdefABCDEF"), {"-"});
    all(characters s, c -> member(c, validChars))
)

loadNode = method()
loadNode(HashTable, String, Function) := (s, key, fn) -> (
    	obj := s#"obj";
    	
	if (instance(obj, String) and isUUID(obj)) then return loadRef(s);
	
    	s#"obj" = s#"obj"#key;
	result := fn(s);
	s#"obj" = obj;
	return result;
    )

loadNode(HashTable, ZZ, Function) := (s, key, fn) -> (
    obj := s#"obj";
    if (instance(obj, String) and isUUID(obj)) then return loadRef(s);
    s#"obj" = s#"obj"#key;
    result := fn(s);
    s#"obj" = obj;
    return result;
    )

-- Function to load a reference from a deserialized state
loadRef = method()
loadRef(HashTable) := s -> (
    -- Get the ID from the current object
    id := s#"obj";
    local loadedRef;

    -- Check if the ID already exists in the local state
    -- TODO update for global state functionality
    if member(id, keys s#"idToObj") then (
        -- If it exists, retrieve the object
        loadedRef = s#"idToObj"#id
    ) else (
        -- Otherwise, load the object from references
        -- Set object to the reference with this ID
        s#"obj" = s#"refs"#id;
        
        -- Load the object
        loadedRef = loadTypedObject(s);
        
        -- Store the loaded object in the global state
        s#"idToObj"#id = loadedRef;
        s#"objToId"#loadedRef = id;
    );
    -- Return the loaded reference
    return loadedRef
)


-- The decode_type function in Macaulay2
decodeType = method()
decodeType(HashTable) := s -> (
    if instance(s#"obj", String) then (
        if (isUUID(s#"obj")) then (
            id := s#"obj";
            obj := s#"obj";
	    if (s#"refs" === null) then (
                return class(s#"idToObj"#id);
            );
            s#"obj" = s#"refs"#id;
            T := decodeType(s);
            s#"obj" = obj;
            return T;
        ) else (
	key := s#"obj";
	if member(key, keys reverseTypeMap) then (
	    return reverseTypeMap#key
	    ) else (
	    error("unsupported type for decoding " | key);
	    )
        )
    );

    if member("_type", keys s#"obj") then (
	return loadNode(s, "_type", decodeType)
	);
    
    if member("name", keys s#"obj") then (
	return loadNode(s, "name", decodeType);
    )
);

loadTypeParams = method()
loadTypeParams(MutableHashTable, Type, String) := (s, T, key) -> (
    return loadNode(s, key, s -> loadTypeParams(s, T))
    )	 
loadTypeParams(MutableHashTable, Type) := (s, T) -> (
    -- If the object is a string
    if instance(s#"obj", String) then (
        -- Check if it's a UUID
        if isUUID(s#"obj") then (
            return (T, loadRef(s))
        );
        return (T, null)
    );
    
    -- If object has params key
    if member("params", keys s#"obj") then (
	getParams = localState -> (
	    obj = localState#"obj";
	    -- Handle array params
	    if instance(obj, List) then (
		print "please implement loading list of params";
      	    	) else if instance(obj, String) or member("params", keys obj) then (
    	    	if instance(obj, String) and isUUID(obj) then (
		    return (T, loadRef(localState));
		    );	 
		if member(obj, keys singletonTypes) then (
		    params = singletonTypes#obj;
		    ) else (
		    U = decodeType(localState);
		    tp = loadTypeParams(localState, U);
		    params = tp#1;
		    )
		) else if not member("_type", keys obj) then (
		    -- the case where "_type" is not a key, means their are multiple parameters
		    -- when obj is a hash table it means they are named parameters
		    params := new MutableHashTable;
		    scan(keys obj, k -> (
    			    params#k = loadNode(localState, k, localState -> (
    	    	    	    	    obj = localState#"obj";
				    if instance(obj, List) or instance(obj, Array) then (
				    	return loadTypeArrayParams(localState);
				    	);
				    U := decodeType(localState);
				    -- Check if object is a string and not a UUID
				    if instance(obj, String) and not isUUID(obj) then (
				    	return U;
				    	);
				    if U === null then (
					--- ignore this part of the type tree
					return null;
					);
				    return (loadTypeParams(localState, U))#1;
				    ))
			    ))
		    ) else (
		    params = loadTypedObject(localState);
            	    );
    	    	return (T, params);
	    	);	  
	    return loadNode(s, "params", getParams);
	) else (
	return (T, loadTypedObject(s));
	);
    );

loadObject = method()
loadObject(HashTable, Type, Thing) := (s, T, params) -> (
    if T === PolynomialRing then (
	if member("grading_group", keys params) then (
	    	R = params#"ring";
		return baseRing(R)[gens(R), Degrees=>apply(s#"obj", x -> apply(x#0, value))];
	    );
	symbols = s#"obj"#"symbols";
	    return params[symbols];
	);
    if T === RingElement then (
	if instance(s#"obj", String) then return (value s#"obj")_params;
    	p = 0_params;
	BR = baseRing(params);
	for i in 0 .. (length s#"obj" - 1) do (
	    coeffExp = loadNode(s, i, s -> (
		    term = s#"obj";
	    	    if instance(term#1, String) then (
			return {apply(term#0, x -> value x), (value term#1)_BR};
			) else (
			coeff = loadNode(s, 1, s -> loadObject(s, RingElement, BR));
    	    		return {apply(term#0, x -> value x), coeff};		
			);
		    ));
	    p = p + coeffExp#1 * params_(coeffExp#0); 
	    );	 
	print p;
	return p;
	);    
    
    error("loadObject has not been extended to handle " | toString(T));
    )
loadObject(HashTable, List, Thing) := (s, typeList, params) -> (
    if typeList#0 === List then (
    	for i from 0 to length s#"obj" list (
	    if 2 == length typeList then loadNode(s, i, s -> loadObject(s, typeList#1, params);
	));
        error("loadObject has not been extended to handle " | toString(typeList));    
    )
)

loadTypedObject = s -> (
    	T = decodeType(s);
    	tp = loadTypeParams(s, T, "_type");
    	T = tp#0;
	params = tp#1;
	if member("data", keys s#"obj") then (
    	    return loadNode(s, "data", s -> loadObject(s, T, params));
	    ) else (
	    	return singletonTypes#(s#"obj"#"_type");
	    );
    );

loadMRDIJSON = filename -> (
    -- this should become a global serializer state to save many objects in the
    -- same session
    jsonHT = fromJSON openIn filename;
    state := new MutableHashTable from {
	"objToId" => new MutableHashTable,
	"idToObj" => new MutableHashTable,
	"obj" => jsonHT,
	"refs" => if member("_refs", keys jsonHT) then jsonHT#"_refs" else null
	};

    return loadTypedObject(state);
    );



--end--






