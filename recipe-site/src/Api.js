export function postRecipe(name, tags, files) {
  const formData = new FormData()
  formData.append("name", name)
  formData.append("tags", tags)
  for (const file of files) {
    formData.append("images", file, file.name)
  }
  return fetch("/api/recipes/", {
    method: "POST",
    cache: "no-cache",
    body: formData
  })
    .then(res => res.json())
}

export function deleteRecipe(id) {
  console.log("Deleting recipe", id)
  return fetch("/api/recipes/" + id, {
    method: "DELETE",
  })
}

// page is 0-indexed.
export function searchRecipes(query="", page=0) {
  const PAGE_SIZE = 20
  let params = new URLSearchParams({
    query,
    limit: PAGE_SIZE,
    offset: page * PAGE_SIZE,
  })
  return fetch("/api/recipes/?" + params)
      .then(res => res.json())
}

export function getRecipe(id) {
  return fetch("/api/recipes/" + id)
    .then(res => res.json())
}

export function updateRecipe(id, {name, tags, files}) {
  const formData = new FormData()
  formData.set("name", name)
  formData.set("tags", tags)

  for (const file of files) {
    formData.append("images", file, file.name)
  }

  return fetch("/api/recipes/" + id, {
    method: "PUT",
    cache: "no-cache",
    body: formData
  })
}
