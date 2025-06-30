def normalize(str)
  str.downcase.gsub(/[^a-z0-9 ]/, '')
end

def tokenize(str)
  normalize(str).split.uniq.to_set
end

def jaccard(a, b)
  return 0.0 if (a | b).empty?
  (a & b).size.to_f / (a | b).size
end

def naive_overlap(a, b)
  (a & b).size
end

def levenshtein(a, b)
  a_len = a.length
  b_len = b.length
  d = Array.new(a_len + 1) { |i| [i] + [0] * b_len }

  (1..b_len).each { |j| d[0][j] = j }

  (1..a_len).each do |i|
    (1..b_len).each do |j|
      cost = a[i - 1] == b[j - 1] ? 0 : 1
      d[i][j] = [
        d[i - 1][j] + 1,
        d[i][j - 1] + 1,
        d[i - 1][j - 1] + cost
      ].min
    end
  end

  d[a_len][b_len]
end

require 'set'

TRANSACTIONS = [
  "ORANGE SA-ORANGE",
  "ORANGE MOBILE BILL",
  "NETFLIX 12/06",
  "SPAR PARIS 10/05",
  "SPOTIFY FAMILY PLAN",
  "AMAZON FRANCE SAS"
]

print "Enter a transaction description to compare:\n> "
input = gets.chomp

input_tokens = tokenize(input)

scored = TRANSACTIONS.map do |t|
  tokens = tokenize(t)
  [
    t,
    jaccard(input_tokens, tokens),
    naive_overlap(input_tokens, tokens),
    levenshtein(normalize(input), normalize(t))
  ]
end

puts "\nBy Jaccard similarity:"
scored.sort_by { |_, sim, _, _| -sim }.first(3).each { |row| p row }

puts "\nBy naive word overlap:"
scored.sort_by { |_, _, overlap, _| -overlap }.first(3).each { |row| p row }

puts "\nBy Levenshtein distance (lower is better):"
scored.sort_by { |_, _, _, dist| dist }.first(3).each { |row| p row }


require 'pry'; binding.pry
