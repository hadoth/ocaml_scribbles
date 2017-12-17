let rec replicateIt (toReplicate: 'a) (count: int) (result: 'a list) = if count < 0 then raise (Failure "Replication count must not be negative") else if count = 0 then result else replicateIt toReplicate (count - 1) (toReplicate::result);;
let replicate (count: int) (toReplicate: 'a) = replicateIt toReplicate count [];;

replicate 4 8;;
replicate 8 4;;
