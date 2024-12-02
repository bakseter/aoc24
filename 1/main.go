package main

import (
	"math"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}

	lines := strings.Split(strings.TrimSpace(string(file)), "\n")
	firstList, secondList := getTwoLists(lines)

	// Part 1
	var difference []int
	for index, firstElement := range firstList {
		secondElement := secondList[index]
		diff := int(math.Abs(float64(secondElement - firstElement)))
		difference = append(difference, diff)
	}

	println(sum(difference))

	// Part 2
	var similarityScores []int
	for _, n := range firstList {
		score := similarityScore(n, secondList)
		similarityScores = append(similarityScores, score)
	}

	println(sum(similarityScores))
}

func sum(list []int) int {
	var sum int
	for _, n := range list {
		sum += n
	}
	return sum
}

func similarityScore(num int, list []int) int {
	var score int
	for _, n := range list {
		if n == num {
			score++
		}
	}
	return score * num
}

func getTwoLists(lines []string) ([]int, []int) {
	var list1, list2 []int
	for _, line := range lines {
		words := strings.Fields(line)
		firstWord := words[0]
		secondWord := words[1]

		n, err := strconv.Atoi(firstWord)
		if err != nil {
			panic(err)
		}
		m, err := strconv.Atoi(secondWord)
		if err != nil {
			panic(err)
		}

		list1 = append(list1, n)
		list2 = append(list2, m)
	}

	slices.Sort(list1)
	slices.Sort(list2)

	return list1, list2
}
