<template>
  <page-skeleton :breadcrumbs="breadcrumbs">
    <template #breadcrumbs>
      <el-breadcrumb-item :to="{ name: 'recipe-list' }">
        My Recipes
      </el-breadcrumb-item>
      <el-breadcrumb-item>
        {{ recipe.name }}
      </el-breadcrumb-item>
    </template>
    <template #header-right>
      <el-button
        @click="goToEdit"
        type="primary">Edit</el-button>
    </template>
    <h1>{{recipe.name}}</h1>
    <div>
      <el-image
        v-for="url in recipe.images"
        :key="url"
        fit="fill"
        :src="url"></el-image>
    </div>
    <el-collapse>
      <el-collapse-item title="Content">
        <p v-for="(text, i) in paragraphs" :key="i">{{text}}</p>
      </el-collapse-item>
    </el-collapse>
  </page-skeleton>
</template>

<script>
import PageSkeleton from "./PageSkeleton.vue"
import { getRecipe } from "../Api.js"

export default {
  name: 'RecipeView',
  components: {PageSkeleton},
  data() {
    return {
      recipe: {
        name: "",
        images: [],
        body: "",
      },
    }
  },
  computed: {
    paragraphs() {
      return this.$data.recipe.body
        .split(/\n+/g)
        .filter(str => str != "")
    },
  },
  methods: {
    goToEdit() {
      this.$router.push({ name: "recipe-edit" })
    },
  },
  created() {
    getRecipe(this.$route.params.id)
      .then(recipe => {
        this.$data.recipe = recipe
      })
  },
}
</script>

<style>
</style>
