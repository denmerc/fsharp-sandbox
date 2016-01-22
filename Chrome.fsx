(*** hide ***)
#load "packages/FsLab/FsLab.fsx"
(**
FsLab Experiment
================
*)

        
    type Chromosome = char []
    type Fitness = int
    type Individual = Chromosome * Fitness

        
    let target : Chromosome = [|'d'; 'e'; 'n'; 'n'; 'i'; 's'; 'f'; 'h'; 'h'; 'e'; 'e'; 'e'|]

    let eliteSize = 10

    let delta (x: char) (y: char) =
        abs( (int)x - (int)y ) 

    let calculateFitness (target: Chromosome) (chromosome: Chromosome) =
        Array.map2 (fun x y -> delta x y) target chromosome
        |> Array.sum

    let getFitness = calculateFitness target

    let rnd = System.Random()

    let createChromosome size : Individual =
        let chromosome = Array.init<char> size (fun _ -> (char) (rnd.Next(90, 123)) )
        let fitness = getFitness chromosome
        (chromosome, fitness)

    let initializePopulation withSize =
        Array.init<Individual> withSize (fun _ -> createChromosome target.Length)
        |> Array.sortBy (fun (_, f) -> f)

    let getElite (population: Individual []) =
        let size = population.Length / eliteSize
        population.[..size]
    
    let mutateChromosome(chromosome: Chromosome) =
        let index = rnd.Next(target.Length)
        let allele = Array.init<char> 1 (fun _ -> (char) (rnd.Next(90, 123)))
        let result = if index > 0 then Array.append chromosome.[..index-1] allele else allele
        let result = if index >= target.Length-1 then result else Array.append result chromosome.[index + 1..]
        let fitness = getFitness result
//        printfn "Mutate (%d): %A" fitness result     
        (result, fitness)

    let mutate (population: Individual []) (prob: int) =
        let newPop = population |> Array.map (fun (x,y) -> if (rnd.Next(0,100) < prob) then mutateChromosome x else (x,y))
        newPop

    let crossover (population: Individual []) =
        let top50percent = population.Length / 2
        let mom = fst population.[rnd.Next(top50percent)]
        let dad = fst population.[rnd.Next(top50percent)]

        let index = rnd.Next(target.Length)

        let chromosome = Array.append mom.[..index] dad.[index + 1..]
        let fitness = getFitness chromosome
        (chromosome, fitness)

    let newGeneration population prob =
        let elite = getElite population
        let withSize = population.Length - elite.Length
        Array.init<Individual> withSize (fun _ -> crossover population)
        |> (fun (p) -> mutate p prob)
        |> Array.append elite
        |> Array.sortBy (fun (_, f) -> f)


    let printBest (population: Individual []) (cycle: int) =
        (fst population.[0]) |> (fun x -> System.String x) |> printfn "%d Best (%d): %A" cycle (snd population.[0])

    let evolve population prob =

        let rec evolve population n =
            printBest population n

            match (n, (snd population.[0])) with
            | (0, _) -> population.[0]
            | (_, 0) -> population.[0]
            | _ ->
                let newPopulation = newGeneration population prob
                evolve newPopulation (n - 1)

        evolve population 300

//    [<EntryPoint>]
    let main args =
        printfn "Evolve..."


        

        let population = initializePopulation 80
        
        let best = evolve population 15
        printfn "%d" population.Length
//        System.Console.ReadLine() |> ignore
        0