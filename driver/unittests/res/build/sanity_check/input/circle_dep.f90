module mod1
    use t
end module mod1

module mod2
    use mod1
end module mod2

module mod3
    use mod2
end module mod3

module t
    use mod3
end module t
