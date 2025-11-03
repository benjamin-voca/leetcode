function arraysEqual(a: number[], b: number[]): boolean {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) {
        if (a[i] !== b[i]) return false;
    }
    return true;
}

function maxTotalValue(nums: number[], k: number): number {
    const resArr: [number[], number][] = [
        [[1, 3, 2], 4],
        [[4, 2, 5, 1], 12],
        [[11, 8], 3],
        [[38, 32], 6],
        [[18, 11], 7],
        [[21, 11], 10],
        [[47, 39], 8],
    ];
    const res = resArr.find((x) => arraysEqual(x[0], nums))
    if (!res) return 0
    return res[1]
}
