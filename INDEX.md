# 📚 LeetCode Haskell Project - Documentation Index

## 🎯 Quick Navigation

### For New Users
1. **START_HERE.md** - Begin here! Overview and quick start
2. **TASTY_SETUP.md** - Complete setup guide with examples
3. **WORKFLOW.md** - Visual step-by-step workflow

### For Developers
4. **TESTING_WITH_TASTY.md** - Comprehensive testing guide
5. **QUICKSTART_TASTY.md** - Quick reference for patterns
6. **README.md** - Full project documentation

---

## 📖 What Each Document Covers

### START_HERE.md
- Project overview
- 3-step quick start
- Key features
- Documentation roadmap

### TASTY_SETUP.md
- Features overview
- Project structure
- Quick start examples
- Common patterns
- Pro tips

### WORKFLOW.md
- Visual diagrams
- File dependencies
- Execution flow
- Common patterns
- Problem naming

### TESTING_WITH_TASTY.md
- Tasty framework overview
- Test structure
- Writing tests
- Running tests
- Best practices
- Troubleshooting

### QUICKSTART_TASTY.md
- Creating problems
- Implementing solutions
- Writing tests
- Tasty patterns
- Example workflows

### README.md
- Full project description
- Feature list
- Commands
- Dependencies
- Tips & tricks

---

## 🚀 One-Minute Setup

```bash
# 1. Create problem
python3 scripts/new-problem.py 1 "solution :: [Int] -> Int"

# 2. Implement (src/Solutions/P1.hs)
# 3. Test (test/Problems/P1.hs)
# 4. Run
cabal test
```

---

## 📋 Common Tasks

| Task | File | Command |
|------|------|---------|
| Start using project | START_HERE.md | Read first! |
| Create new problem | WORKFLOW.md | `python3 scripts/new-problem.py ...` |
| Write tests | TESTING_WITH_TASTY.md | Edit test/Problems/P*.hs |
| View examples | QUICKSTART_TASTY.md | Copy-paste patterns |
| Run tests | README.md | `cabal test` |
| Understand workflow | WORKFLOW.md | Read diagrams |

---

## 🎓 Recommended Reading Order

```
1. START_HERE.md
   ↓
2. TASTY_SETUP.md
   ↓
3. Try: python3 scripts/new-problem.py 1 "yourFunc :: Int -> Int"
   ↓
4. WORKFLOW.md (visual guide)
   ↓
5. TESTING_WITH_TASTY.md (detailed reference)
   ↓
6. QUICKSTART_TASTY.md (patterns & examples)
   ↓
7. README.md (complete documentation)
```

---

## ✨ Key Features

- **Automatic problem generation** - One command creates everything
- **Tasty testing** - Clean, organized test suites
- **Auto-cabal updates** - No manual configuration needed
- **Generic helpers** - Reusable utilities
- **Beautiful output** - Colored test results

---

## 📁 Project Structure

```
leetcode/
├── src/Solutions/         ← Your solutions
├── test/Problems/         ← Your tasty tests
├── scripts/new-problem.py ← CREATE NEW PROBLEMS
└── [Documentation]
    ├── START_HERE.md          ← Read first!
    ├── TASTY_SETUP.md
    ├── WORKFLOW.md
    ├── TESTING_WITH_TASTY.md
    ├── QUICKSTART_TASTY.md
    ├── README.md
    └── INDEX.md (this file)
```

---

## 🚀 Get Started Now

1. Read START_HERE.md
2. Run: python3 scripts/new-problem.py 1 "twoSum :: [Int] -> Int -> [Int]"
3. Edit src/Solutions/P1.hs
4. Edit test/Problems/P1.hs with tasty tests
5. Run: cabal test

---

## ❓ Quick Q&A

**Q: Where do I start?**  
A: Read START_HERE.md then TASTY_SETUP.md

**Q: How do I create a new problem?**  
A: python3 scripts/new-problem.py <num> "<signature>"

**Q: How do I write tests?**  
A: See TESTING_WITH_TASTY.md or copy examples from QUICKSTART_TASTY.md

**Q: How do I run tests?**  
A: cabal test

**Q: What's the project structure?**  
A: See WORKFLOW.md for diagrams

---

## 📞 Document Purposes

| Doc | Purpose | Best For |
|-----|---------|----------|
| START_HERE | Overview & quick start | Getting oriented |
| TASTY_SETUP | Feature showcase | Understanding capabilities |
| WORKFLOW | Visual guide | Understanding flow |
| TESTING_WITH_TASTY | Deep reference | Learning testing |
| QUICKSTART_TASTY | Copy-paste patterns | Quick solutions |
| README | Complete docs | Full reference |
| INDEX | Navigation | Finding things |

---

**Start here:** START_HERE.md

Happy coding! 🎉
